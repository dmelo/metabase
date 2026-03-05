git reset HEAD~1
rm ./backport.sh
git cherry-pick f02d26aa14a6308273139bf0c2c591c33eff6401
echo 'Resolve conflicts and force push this branch.\n\nTo backport translations run: bin/i18n/merge-translations <release-branch>'
