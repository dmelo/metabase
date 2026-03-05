git reset HEAD~1
rm ./backport.sh
git cherry-pick 1571e58188461279381052abc2e22a361fabce4a
echo 'Resolve conflicts and force push this branch.\n\nTo backport translations run: bin/i18n/merge-translations <release-branch>'
