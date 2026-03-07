(ns metabase.llm.anthropic
  "Anthropic API client for OSS LLM integration.

   Provides synchronous chat completions for text-to-SQL generation
   using tool_use for structured output."
  (:require
   [clj-http.client :as http]
   [clojure.core.memoize :as memoize]
   [clojure.string :as str]
   [metabase.llm.settings :as llm.settings]
   [metabase.util :as u]
   [metabase.util.json :as json])
  (:import
   (com.fasterxml.jackson.core JsonParseException)))

(set! *warn-on-reflection* true)

(def ^:private generate-sql-tool
  "Tool definition for structured SQL output.
   Forces the model to return a JSON object with sql and optional explanation."
  {:name        "generate_sql"
   :description "Generate SQL query from the user's request. Always use this tool to return your response."
   :input_schema {:type       "object"
                  :properties {:sql         {:type        "string"
                                             :description "The generated SQL query"}
                               :explanation {:type        "string"
                                             :description "Brief explanation of the query"}}
                  :required   ["sql"]}})

(def ^:private select-tables-tool
  "Tool definition for structured table selection output.
   Forces the model to return a JSON object with table_ids."
  {:name        "select_tables"
   :description "Select the database tables needed to answer the user's question. Always use this tool to return your response."
   :input_schema {:type       "object"
                  :properties {:table_ids {:type        "array"
                                           :items       {:type "integer"}
                                           :description "IDs of tables needed for the query"}
                               :reasoning {:type        "string"
                                           :description "Brief explanation of why these tables were chosen"}}
                  :required   ["table_ids"]}})

(defn- build-request-headers
  "Build headers for Anthropic API request."
  [api-key]
  {"x-api-key"         api-key
   "anthropic-version" (llm.settings/llm-anthropic-api-version)
   "content-type"      "application/json"})

(defn- build-request-body
  "Build the request body for Anthropic messages API.
   `tool` is the tool definition map to use for structured output."
  [{:keys [model system messages tool]}]
  (let [tool-def (or tool generate-sql-tool)]
    (cond-> {:model       model
             :max_tokens  (llm.settings/llm-max-tokens)
             :messages    messages
             :tools       [tool-def]
             :tool_choice {:type "tool" :name (:name tool-def)}}
      system (assoc :system system))))

(defn- extract-tool-input
  "Extract the tool input from Anthropic messages response.
   Returns the parsed JSON input from the tool_use content block."
  [response-body]
  (let [content (:content response-body)]
    (->> content
         (filter #(= "tool_use" (:type %)))
         first
         :input)))

(defn- handle-api-error
  "Handle HTTP errors from Anthropic API."
  [exception]
  (if-let [response-body (some-> exception ex-data :body)]
    (let [parsed (try
                   (json/decode response-body)
                   (catch JsonParseException _
                     {:error {:message response-body}}))]
      (throw (ex-info (or (-> parsed :error :message)
                          "Anthropic API request failed")
                      {:type   :anthropic-api-error
                       :status (some-> exception ex-data :status)
                       :body   parsed}
                      exception)))
    (throw exception)))

(defn- get-api-key-or-throw
  "Gets Anthropic API key from settings or throws an unconfigured error."
  []
  (let [api-key (llm.settings/llm-anthropic-api-key)]
    (when (str/blank? api-key)
      (throw (ex-info "LLM is not configured. Please set an Anthropic API key via MB_LLM_ANTHROPIC_API_KEY."
                      {:type :llm-not-configured})))
    api-key))

(def ^:private list-models*
  "Memoized implementation of list-models with 5-minute TTL."
  (memoize/ttl
   (fn [api-key]
     (try
       (let [url (str (llm.settings/llm-anthropic-api-url) "/v1/models")
             response (http/get url
                                {:headers {"x-api-key"         api-key
                                           "anthropic-version" (llm.settings/llm-anthropic-api-version)}})
             body (json/decode+kw (:body response))
             models (reverse (sort-by :created_at (:data body)))]
         {:models (map #(select-keys % [:id :display_name]) models)})
       (catch Exception e
         (handle-api-error e))))
   :ttl/threshold (* 5 60 1000)))

(defn list-models
  "Send a list models request to Anthropic.
   Returns a map with :models. Results are cached for 5 minutes."
  []
  (list-models* (get-api-key-or-throw)))

(defn- send-tool-request
  "Send a request to the Anthropic messages API with a specific tool.
   Returns a map with :result (tool input), :usage, and :duration-ms."
  [{:keys [model system messages tool]}]
  (let [model      (or model (llm.settings/llm-anthropic-model))
        request    {:model    model
                    :system   system
                    :messages messages
                    :tool     tool}
        start-time (u/start-timer)]
    (try
      (let [url      (str (llm.settings/llm-anthropic-api-url) "/v1/messages")
            response (http/post url
                                {:headers            (build-request-headers (get-api-key-or-throw))
                                 :body               (json/encode (build-request-body request))
                                 :as                 :json
                                 :content-type       :json
                                 :socket-timeout     (llm.settings/llm-request-timeout-ms)
                                 :connection-timeout (llm.settings/llm-connection-timeout-ms)})
            duration-ms (u/since-ms start-time)
            body        (:body response)
            usage       (:usage body)]
        {:result      (extract-tool-input body)
         :duration-ms duration-ms
         :usage       {:model      model
                       :prompt     (:input_tokens usage)
                       :completion (:output_tokens usage)}})
      (catch Exception e
        (handle-api-error e)))))

(defn chat-completion
  "Send a chat completion request to Anthropic for SQL generation.
   Returns a map with :result (containing :sql), :usage, and :duration-ms."
  [opts]
  (send-tool-request opts))

(defn table-selection
  "Send a request to Anthropic to select relevant tables for a query.
   Returns a map with :result (containing :table_ids and :reasoning), :usage, and :duration-ms."
  [opts]
  (send-tool-request (assoc opts :tool select-tables-tool)))
