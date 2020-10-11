FROM fastai/jekyll:2020-09-12

WORKDIR /fastpages
COPY . .
RUN chmod u+x action_entrypoint.sh
RUN chmod u+x word2post.sh
RUN dos2unix /fastpages/*.sh
RUN pip install -r ./requirements.txt
RUN jupyter nbextension install https://github.com/drillan/jupyter-black/archive/master.zip
RUN jupyter nbextension enable jupyter-black-master/jupyter-black

CMD [ "/fastpages/action_entrypoint.sh" ]
