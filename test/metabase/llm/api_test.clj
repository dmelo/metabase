(ns metabase.llm.api-test
  (:require
   [clojure.set :as set]
   [clojure.string :as str]
   [clojure.test :refer :all]
   [metabase.analytics.snowplow-test :as snowplow-test]
   [metabase.llm.anthropic :as llm.anthropic]
   [metabase.llm.api :as api]
   [metabase.llm.context :as llm.context]
   [metabase.test :as mt]))

(set! *warn-on-reflection* true)

;;; ------------------------------------------- database-dialect Tests -------------------------------------------

(deftest database-engine-test
  (mt/with-temp [:model/Database postgres-db {:engine :postgres}
                 :model/Database mysql-db   {:engine :mysql}
                 :model/Database bigquery-db {:engine :bigquery}]
    (testing "returns engine keyword for database"
      (is (= :postgres (#'api/database-engine (:id postgres-db))))
      (is (= :mysql (#'api/database-engine (:id mysql-db))))
      (is (= :bigquery (#'api/database-engine (:id bigquery-db)))))

    (testing "nil database returns nil"
      (is (nil? (#'api/database-engine nil))))

    (testing "non-existent database returns nil"
      (is (nil? (#'api/database-engine 99999999))))))

;;; ------------------------------------------- load-dialect-instructions Tests -------------------------------------------

(deftest load-dialect-instructions-test
  (testing "known engine with file returns content"
    (let [instructions (#'api/load-dialect-instructions :postgres)]
      (is (string? instructions))
      (is (str/includes? instructions "PostgreSQL"))))

  (testing "nil engine returns nil"
    (is (nil? (#'api/load-dialect-instructions nil)))))

;;; ------------------------------------------- Table ID extraction Tests -------------------------------------------

(deftest extract-frontend-table-ids-test
  (testing "extracts table IDs from referenced_entities"
    (let [entities [{:model "table" :id 1}
                    {:model "table" :id 2}
                    {:model "card" :id 999}
                    {:model "table" :id 3}]]
      (is (= #{1 2 3}
             (->> entities
                  (filter #(= "table" (:model %)))
                  (map :id)
                  set)))))

  (testing "returns empty set for empty input"
    (is (= #{}
           (->> []
                (filter #(= "table" (:model %)))
                (map :id)
                set))))

  (testing "returns empty set when no tables present"
    (let [entities [{:model "card" :id 1}
                    {:model "question" :id 2}]]
      (is (= #{}
             (->> entities
                  (filter #(= "table" (:model %)))
                  (map :id)
                  set))))))

(deftest table-id-union-test
  (testing "all sources merged via set union"
    (let [frontend #{1 2}
          explicit #{2 3}
          implicit #{3 4}]
      (is (= #{1 2 3 4}
             (set/union (or frontend #{})
                        (or explicit #{})
                        (or implicit #{}))))))

  (testing "nil sources treated as empty sets"
    (is (= #{1 2}
           (set/union (or nil #{})
                      (or #{1 2} #{})
                      (or nil #{}))))))

;;; ------------------------------------------- Integration with context Tests -------------------------------------------

(deftest table-mention-parsing-integration-test
  (testing "explicit mentions parsed from prompt"
    (let [prompt "Join [Orders](metabase://table/123) with [Users](metabase://table/456)"]
      (is (= #{123 456}
             (llm.context/parse-table-mentions prompt)))))

  (testing "multiple mentions of same table deduplicated"
    (let [prompt "[Orders](metabase://table/123) and again [Orders](metabase://table/123)"]
      (is (= #{123}
             (llm.context/parse-table-mentions prompt))))))

;;; ------------------------------------------- build-system-prompt Tests -------------------------------------------

(deftest build-system-prompt-test
  (testing "builds prompt with required parameters"
    (let [prompt (#'api/build-system-prompt {:dialect "PostgreSQL"
                                             :schema-ddl "CREATE TABLE users (id INTEGER);"})]
      (is (string? prompt))
      (is (str/includes? prompt "PostgreSQL"))
      (is (str/includes? prompt "CREATE TABLE users"))))

  (testing "includes dialect instructions when provided"
    (let [prompt (#'api/build-system-prompt {:dialect "PostgreSQL"
                                             :schema-ddl "CREATE TABLE users (id INTEGER);"
                                             :dialect-instructions "Use LIMIT instead of TOP"})]
      (is (str/includes? prompt "Use LIMIT instead of TOP"))))

  (testing "includes source SQL when provided"
    (let [prompt (#'api/build-system-prompt {:dialect "PostgreSQL"
                                             :schema-ddl "CREATE TABLE users (id INTEGER);"
                                             :source-sql "SELECT * FROM users"})]
      (is (str/includes? prompt "SELECT * FROM users")))))

;;; ------------------------------------------- Error handling Tests -------------------------------------------

(deftest generate-sql-error-handling-test
  (mt/with-temp [:model/Database db {:engine :postgres}]
    (testing "403 when LLM not configured"
      (mt/with-temporary-setting-values [llm-anthropic-api-key nil]
        (let [response (mt/user-http-request :rasta :post 403 "llm/generate-sql"
                                             {:prompt "test"
                                              :database_id (:id db)})]
          (is (str/includes? (str response) "not configured")))))

    (testing "400 when no tables found via auto-selection (empty database)"
      (mt/with-temporary-setting-values [llm-anthropic-api-key "sk-ant-test"]
        ;; No tables in the database, so auto-selection finds nothing
        (let [response (mt/user-http-request :rasta :post 400 "llm/generate-sql"
                                             {:prompt "no table mentions here"
                                              :database_id (:id db)})]
          (is (str/includes? (str response) "No accessible tables")))))))

(deftest list-models-unconfigured-test
  (testing "Returns 403 when LLM is not configured"
    (mt/with-temporary-setting-values [llm-anthropic-api-key nil]
      (let [response (mt/user-http-request :rasta :get 403 "llm/list-models")]
        (is (str/includes? (str response) "not configured"))))))

;;; ------------------------------------------- Snowplow Tests -------------------------------------------

(defn- token-usage-event? [event]
  (-> event
      :data
      (contains? "total_tokens")))

(defn- simple-event? [event]
  (-> event
      :data
      (contains? "event")))

(deftest generate-sql-snowplow-success-test
  (testing "successful /generate-sql call tracks both token_usage and simple_event"
    (mt/with-temp [:model/Database db {:engine :postgres}
                   :model/Table table {:db_id (:id db) :name "users" :schema "public"}
                   :model/Field _ {:table_id (:id table) :name "id" :base_type :type/Integer}
                   :model/Field _ {:table_id (:id table) :name "name" :base_type :type/Text}]
      (let [mock-chat-response {:result      {:sql "SELECT * FROM users"}
                                :usage       {:model      "claude-sonnet-4-5-20250929"
                                              :prompt     1000
                                              :completion 200}
                                :duration-ms 500}]
        (mt/with-temporary-setting-values [llm-anthropic-api-key "sk-ant-test"]
          (snowplow-test/with-fake-snowplow-collector
            (with-redefs [llm.anthropic/chat-completion (constantly mock-chat-response)]
              (let [response      (mt/user-http-request :rasta :post 200 "llm/generate-sql"
                                                        {:prompt              "get all users"
                                                         :database_id         (:id db)
                                                         :referenced_entities [{:model "table" :id (:id table)}]})
                    events        (snowplow-test/pop-event-data-and-user-id!)
                    token-events  (filter token-usage-event? events)
                    simple-events (filter simple-event? events)]
                (is (= "SELECT * FROM users" (:sql response)))
                (testing "token_usage event"
                  (is (=? [{:data {"model_id"            "claude-sonnet-4-5-20250929"
                                   "prompt_tokens"       1000
                                   "completion_tokens"   200
                                   "total_tokens"        1200
                                   "estimated_costs_usd" 0.0
                                   "duration_ms"         500
                                   "source"              "oss_metabot"
                                   "tag"                 "oss-sqlgen"}}]
                          token-events)))
                (testing "simple_event"
                  (is (=? [{:data {"event"        "metabot_oss_sqlgen_used"
                                   "duration_ms"  int?
                                   "result"       "success"
                                   "event_detail" "postgres"}}]
                          simple-events)))))))))))

(deftest generate-sql-snowplow-failure-test
  (testing "failed /generate-sql call tracks simple_event with failure result"
    (mt/with-temp [:model/Database db {:engine :postgres}
                   :model/Table table {:db_id (:id db) :name "users" :schema "public"}
                   :model/Field _ {:table_id (:id table) :name "id" :base_type :type/Integer}]
      (mt/with-temporary-setting-values [llm-anthropic-api-key "sk-ant-test"]
        (snowplow-test/with-fake-snowplow-collector
          (with-redefs [llm.anthropic/chat-completion (fn [_] (throw (Exception. "API error")))]
            (mt/user-http-request :rasta :post 500 "llm/generate-sql"
                                  {:prompt              "get all users"
                                   :database_id         (:id db)
                                   :referenced_entities [{:model "table" :id (:id table)}]})
            (let [events        (snowplow-test/pop-event-data-and-user-id!)
                  token-events  (filter token-usage-event? events)
                  simple-events (filter simple-event? events)]
              (testing "no token_usage event on failure (chat-completion call failed)"
                (is (empty? token-events)))
              (testing "simple_event with failure result"
                (is (=? [{:data {"event"        "metabot_oss_sqlgen_used"
                                 "duration_ms"  int?
                                 "result"       "failure"
                                 "event_detail" "postgres"}}]
                        simple-events))))))))))

;;; ------------------------------------------- Token Usage Tracking Tests -------------------------------------------

(deftest track-token-usage-with-uuid-test
  (testing "tracks usage with analytics uuid when no premium token is available"
    (let [test-analytics-uuid "test-analytics-uuid-12345"]
      (mt/with-temporary-setting-values [premium-embedding-token nil
                                         analytics-uuid test-analytics-uuid]
        (snowplow-test/with-fake-snowplow-collector
          (#'api/track-token-usage! {:model       "claude-sonnet-4-5-20250929"
                                     :prompt      1000
                                     :completion  500
                                     :duration-ms 1234
                                     :user-id     42
                                     :source      "oss_metabot"
                                     :tag         "oss-sqlgen"})
          (is (=? [{:user-id "42"
                    :data    {"hashed_metabase_license_token" (str "oss__" test-analytics-uuid)
                              "request_id"                    #"[a-h0-9]{32}" ; UUID hex format (no dashes)
                              "model_id"                      "claude-sonnet-4-5-20250929"
                              "total_tokens"                  1500
                              "prompt_tokens"                 1000
                              "completion_tokens"             500
                              "estimated_costs_usd"           0.0
                              "duration_ms"                   1234
                              "source"                        "oss_metabot"
                              "tag"                           "oss-sqlgen"}}]
                  (->> (snowplow-test/pop-event-data-and-user-id!)
                       (filter token-usage-event?)))))))))

(deftest track-token-usage-with-premium-token-test
  (testing "hashes premium token when available"
    (mt/with-random-premium-token! [premium-token]
      (mt/with-temporary-setting-values [premium-embedding-token premium-token]
        (snowplow-test/with-fake-snowplow-collector
          (#'api/track-token-usage! {:model       "claude-sonnet-4-5-20250929"
                                     :prompt      100
                                     :completion  50
                                     :duration-ms 100
                                     :user-id     1
                                     :source      "test"
                                     :tag         "test"})
          ;; Should be a SHA-256 hash (64 hex chars), not "oss__*"
          (is (=? [{:data {"hashed_metabase_license_token" #"[0-9a-f]{64}"}}]
                  (->> (snowplow-test/pop-event-data-and-user-id!)
                       (filter token-usage-event?)))))))))

;;; ------------------------------------------- Auto Table Selection Tests -------------------------------------------

(deftest build-table-selection-prompt-test
  (testing "builds prompt with dialect and tables"
    (let [prompt (#'api/build-table-selection-prompt
                  {:dialect "PostgreSQL"
                   :tables  [{:id 1 :name "users" :schema "public"
                              :description "App users" :column_names "id, name, email"}
                             {:id 2 :name "orders" :schema nil
                              :description nil :column_names "id, user_id, total"}]})]
      (is (string? prompt))
      (is (str/includes? prompt "PostgreSQL"))
      (is (str/includes? prompt "users"))
      (is (str/includes? prompt "orders"))
      (is (str/includes? prompt "id, name, email")))))

(deftest auto-select-tables-test
  (mt/with-test-user :crowberto
    (mt/with-temp [:model/Database db    {:engine :postgres}
                   :model/Table    t1    {:db_id (:id db) :name "users" :schema "public"}
                   :model/Field    _f1   {:table_id (:id t1) :name "id" :database_type "INTEGER" :base_type :type/Integer}
                   :model/Table    t2    {:db_id (:id db) :name "orders" :schema "public"}
                   :model/Field    _f2   {:table_id (:id t2) :name "id" :database_type "INTEGER" :base_type :type/Integer}]
      (testing "returns valid table IDs selected by LLM"
        (let [mock-response {:result      {:table_ids [(:id t1) (:id t2)]
                                           :reasoning "Need users and orders"}
                             :usage       {:model "claude-sonnet-4-5-20250929"
                                           :prompt 200 :completion 50}
                             :duration-ms 100}]
          (snowplow-test/with-fake-snowplow-collector
            (with-redefs [llm.anthropic/table-selection (constantly mock-response)]
              (let [result (#'api/auto-select-tables (:id db) "show me users with orders" "PostgreSQL")]
                (is (= #{(:id t1) (:id t2)} result)))))))

      (testing "filters out hallucinated table IDs"
        (let [mock-response {:result      {:table_ids [(:id t1) 999999]
                                           :reasoning "Found users"}
                             :usage       {:model "claude-sonnet-4-5-20250929"
                                           :prompt 200 :completion 50}
                             :duration-ms 100}]
          (snowplow-test/with-fake-snowplow-collector
            (with-redefs [llm.anthropic/table-selection (constantly mock-response)]
              (let [result (#'api/auto-select-tables (:id db) "show me users" "PostgreSQL")]
                (is (= #{(:id t1)} result))
                (is (not (contains? result 999999))))))))

      (testing "throws when LLM returns no valid table IDs"
        (let [mock-response {:result      {:table_ids [999999]}
                             :usage       {:model "claude-sonnet-4-5-20250929"
                                           :prompt 200 :completion 50}
                             :duration-ms 100}]
          (snowplow-test/with-fake-snowplow-collector
            (with-redefs [llm.anthropic/table-selection (constantly mock-response)]
              (is (thrown-with-msg?
                   clojure.lang.ExceptionInfo
                   #"Could not identify relevant tables"
                   (#'api/auto-select-tables (:id db) "gibberish" "PostgreSQL"))))))))))

(deftest auto-select-tables-empty-db-test
  (mt/with-test-user :crowberto
    (mt/with-temp [:model/Database db {:engine :postgres}]
      (testing "throws when database has no accessible tables"
        (snowplow-test/with-fake-snowplow-collector
          (is (thrown-with-msg?
               clojure.lang.ExceptionInfo
               #"No accessible tables"
               (#'api/auto-select-tables (:id db) "test" "PostgreSQL"))))))))

(deftest auto-select-tables-cap-test
  (mt/with-test-user :crowberto
    (testing "caps auto-selected tables at max-auto-selected-tables"
      (let [max-cap        @#'api/max-auto-selected-tables
            ;; Create fake table summaries with IDs 1..(max-cap + 5)
            fake-ids       (vec (range 1 (+ max-cap 6)))
            fake-summaries (mapv (fn [id] {:id id :name (str "t" id) :column_names "id"})
                                 fake-ids)
            mock-response  {:result      {:table_ids fake-ids
                                          :reasoning "All tables"}
                            :usage       {:model "claude-sonnet-4-5-20250929"
                                          :prompt 200 :completion 50}
                            :duration-ms 100}]
        (snowplow-test/with-fake-snowplow-collector
          (with-redefs [llm.context/build-table-summary-context (constantly fake-summaries)
                        llm.anthropic/table-selection           (constantly mock-response)]
            (let [result (#'api/auto-select-tables 1 "everything" "PostgreSQL")]
              (is (<= (count result) max-cap))
              (is (every? (set fake-ids) result)))))))))

(deftest generate-sql-auto-selection-integration-test
  (testing "generate-sql falls through to auto-selection when no tables specified"
    (mt/with-temp [:model/Database db    {:engine :postgres}
                   :model/Table    table {:db_id (:id db) :name "users" :schema "public"}
                   :model/Field    _f1   {:table_id (:id table) :name "id" :database_type "INTEGER" :base_type :type/Integer}
                   :model/Field    _f2   {:table_id (:id table) :name "name" :database_type "VARCHAR" :base_type :type/Text}]
      (let [table-selection-called? (atom false)
            mock-table-selection    {:result      {:table_ids [(:id table)]}
                                     :usage       {:model "claude-sonnet-4-5-20250929"
                                                   :prompt 200 :completion 50}
                                     :duration-ms 100}
            mock-chat-completion    {:result      {:sql "SELECT * FROM users"}
                                     :usage       {:model "claude-sonnet-4-5-20250929"
                                                   :prompt 1000 :completion 200}
                                     :duration-ms 500}]
        (mt/with-temporary-setting-values [llm-anthropic-api-key "sk-ant-test"]
          (snowplow-test/with-fake-snowplow-collector
            (with-redefs [llm.anthropic/table-selection (fn [_]
                                                          (reset! table-selection-called? true)
                                                          mock-table-selection)
                          llm.anthropic/chat-completion (constantly mock-chat-completion)]
              (let [response (mt/user-http-request :rasta :post 200 "llm/generate-sql"
                                                   {:prompt      "show me all users"
                                                    :database_id (:id db)})]
                (is @table-selection-called?
                    "auto-selection should be called when no tables specified")
                (is (= "SELECT * FROM users" (:sql response)))))))))))

(deftest generate-sql-skips-auto-selection-with-explicit-tables-test
  (testing "generate-sql does NOT auto-select when tables are explicitly provided"
    (mt/with-temp [:model/Database db    {:engine :postgres}
                   :model/Table    table {:db_id (:id db) :name "users" :schema "public"}
                   :model/Field    _f1   {:table_id (:id table) :name "id" :database_type "INTEGER" :base_type :type/Integer}]
      (let [table-selection-called? (atom false)
            mock-chat-completion    {:result      {:sql "SELECT * FROM users"}
                                     :usage       {:model "claude-sonnet-4-5-20250929"
                                                   :prompt 1000 :completion 200}
                                     :duration-ms 500}]
        (mt/with-temporary-setting-values [llm-anthropic-api-key "sk-ant-test"]
          (snowplow-test/with-fake-snowplow-collector
            (with-redefs [llm.anthropic/table-selection (fn [_]
                                                          (reset! table-selection-called? true)
                                                          nil)
                          llm.anthropic/chat-completion (constantly mock-chat-completion)]
              (mt/user-http-request :rasta :post 200 "llm/generate-sql"
                                    {:prompt              "show me all users"
                                     :database_id         (:id db)
                                     :referenced_entities [{:model "table" :id (:id table)}]})
              (is (not @table-selection-called?)
                  "auto-selection should NOT be called when tables are explicitly provided"))))))))
