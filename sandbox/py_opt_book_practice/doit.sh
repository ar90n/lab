#! /bin/bash
git submodule update
mkdir -p 2.tutorial 3.school 4.coupon 5.routing 6.api
cp PyOptBook/2.tutorial/*.csv ./2.tutorial

poetry install