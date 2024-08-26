#!/usr/bin/bash

curl -o .devcontainer/judge.zip -L https://mncore-challenge.preferred.jp/judge.zip 
unzip -d .devcontainer/ .devcontainer/judge.zip
rm .devcontainer/judge.zip

echo  "
RUN apt -y install curl
COPY <<EOF /usr/local/bin/judge
#!/usr/bin/bash

python3 /judge-py/judge.py \\\$@
EOF
RUN chmod +x /usr/local/bin/judge
" >> .devcontainer/judge/Dockerfile