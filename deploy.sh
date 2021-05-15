echo "Running tests..."
if (project/cbl test); then
  source private/env.sh
  echo "Tests passed. Deploying to server..."
  rsync -rvlpt -e "ssh -i private/student2021.pem" project/src project/modules project/cbl project/main-program.cbl ubuntu@$SERVER:/var/sftp/deploy/
  echo "Done."
else
  echo "Tests failed. Not deploying."
  exit 1
fi


