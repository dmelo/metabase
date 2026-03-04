git reset HEAD~1
rm ./backport.sh
git cherry-pick 9c44b338870b0fa075a8017d5f1cd601cbeefd9d
echo 'Resolve conflicts and force push this branch.\n\nTo backport translations run: bin/i18n/merge-translations <release-branch>'
