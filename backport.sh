git reset HEAD~1
rm ./backport.sh
git cherry-pick ab7e4712b12ed1a32dc345832a1e065e41475de8
echo 'Resolve conflicts and force push this branch.\n\nTo backport translations run: bin/i18n/merge-translations <release-branch>'
