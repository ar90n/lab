const https = require('https');
const AWS = require("aws-sdk");
const urlParse = require("url").URL;
const meta = require("../../../amplify-meta");
const appsyncUrl = 'https://uoobkxjgbvfo7pmb33joyftfeu.appsync-api.ap-northeast-1.amazonaws.com/graphql';
const region = 'ap-northeast-1';
const endpoint = new urlParse(appsyncUrl).hostname.toString();
const graphqlQuery = require('./query.js').mutation;
const apiKey = process.env.API_KEY;

exports.handler = async (event) => {
  console.log('Received S3 event:', JSON.stringify(event, null, 2));
  const bucket = event.Records[0].s3.bucket.name; //eslint-disable-line
  const key = event.Records[0].s3.object.key; //eslint-disable-line
  console.log(`Bucket: ${bucket}`, `Key: ${key}`);
  console.log(`AppSync URL: ${appsyncUrl}`);
  console.log(`Region: ${region}`);

  const req = new AWS.HttpRequest(appsyncUrl, region);

  const item = {
    input: {
      bucket,
      key,
      description: "Request Generated from Lambda"
    }
  };

  req.method = "POST";
  req.headers.host = endpoint;
  req.headers["Content-Type"] = "application/json";
  req.body = JSON.stringify({
    query: graphqlQuery,
    operationName: "createRequest",
    variables: item
  });

  if (apiKey) {
    req.headers["x-api-key"] = apiKey;
  } else {
    const signer = new AWS.Signers.V4(req, "appsync", true);
    signer.addAuthorization(AWS.config.credentials, AWS.util.date.getDate());
  }

  const data = await new Promise((resolve, reject) => {
    const httpRequest = https.request({ ...req, host: endpoint }, (result) => {
      result.on('data', (data) => {
        resolve(JSON.parse(data.toString()));
      });
    });

    httpRequest.write(req.body);
    httpRequest.end();
  });

  return {
    statusCode: 200,
    body: data
  };
};
