'use strict';

const axios = require('axios');
const url = require('locutus/php/url');

require('dotenv').config();

const LINE_LOGIN_CLIENT_ID = process.env.CHANNEL_ID;
const LINE_LOGIN_CLIENT_SECRET = process.env.CHANNEL_SECRET;
const LINE_LOGIN_REDIRECT_ENCODED_URL = encodeURIComponent( process.env.REDIRECT_URL );
const LINE_LOGIN_PAGE_URL = `https://access.line.me/dialog/oauth/weblogin?response_type=code&client_id=${LINE_LOGIN_CLIENT_ID}&redirect_uri=${LINE_LOGIN_REDIRECT_ENCODED_URL}`;
const LINE_LOGIN_ACCESS_TOKEN_URL = 'https://api.line.me/v1/oauth/accessToken';
const LINE_LOGIN_VALIDATE_URL = 'https://api.line.me/v1/oauth/verify';
const LINE_LOGIN_PROFILE_URL = 'https://api.line.me/v1/profile';

module.exports.auth = (event, context, callback) => {
    const referer = event.headers.Referer;
    const query = event.queryStringParameters;
    if( ( referer !== LINE_LOGIN_PAGE_URL ) || !query || !query.code )
    {
        const response = {
            statusCode: 303,
            headers: {
                Location: LINE_LOGIN_PAGE_URL
            }
        };
        return callback(null, response);
    }

    const body = {
        grant_type: 'authorization_code',
        client_id: LINE_LOGIN_CLIENT_ID,
        client_secret: LINE_LOGIN_CLIENT_SECRET,
        code: query.code,
        redirect_uri: LINE_LOGIN_REDIRECT_ENCODED_URL
    };
    const body_query = url.http_build_query( body );
    const headers = {
        'Content-Type': 'application/x-www-form-urlencoded',
        'Content-Length': body_query.length,
    };

    axios.create({headers})
        .post(LINE_LOGIN_ACCESS_TOKEN_URL, body_query)
        .then((api_response) => {
            const headers = { 'Authorization': `Bearer {${api_response.data.access_token}}` };
            return axios.create({headers}).get(LINE_LOGIN_VALIDATE_URL).then( () => {;
                return axios.create({headers}).get(LINE_LOGIN_PROFILE_URL);
            });
        }).then( (content) => {
            const response = {
                statusCode: 200,
                body: JSON.stringify( content.data )
            };
            callback(null, response);
        });
};
