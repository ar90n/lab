'use strict';

const axios = require('axios');

// Your first function handler
module.exports.webhook = (event, context, callback) => {
    const replyToken = event.body.events[0].replyToken;
    const accessToken = '<access-token>'; // TODO: replace this token before deployment

    const quotes = [
        'Don\'t cry because it\'s over, smile because it happened. - Dr. Seuss',
        'Be yourself; everyone else is already taken. - Oscar Wilde',
        'Two things are infinite: the universe and human stupidity; and I\'m not sure about the universe. - Albert Einstein',
        'Be who you are and say what you feel, because those who mind don\'t matter, and those who matter don\'t mind. - Bernard M. Baruch',
        'So many books, so little time. - Frank Zappa',
        'A room without books is like a body without a soul. - Marcus Tullius Cicero'
            ];

    const randomQuote = quotes[Math.floor(Math.random() * quotes.length)];

    const url = 'https://api.line.me/v2/bot/message/reply';
    const payload = {
        replyToken: replyToken,
        messages: [{type: "text", text: randomQuote}]
    };
    const headers = {
        "Content-type": "application/json",
        "Content-Length": payload.length,
        "Authorization": `Bearer {${accessToken}}`
    };

    axios.create({headers}).post(url, payload).then((response) => callback(null, response));
};
