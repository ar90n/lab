'use strict';

const LINEBot = require('line-messaging');
require('dotenv').config();

var bot = LINEBot.create({
    channelID: process.env.CHANNEL_ID,
    channelSecret: process.env.CHANNEL_SECRET,
    channelToken: process.env.CHANNEL_ACCESS_TOKEN
});

module.exports.webhook = (event, context, callback) => {
    bot.on(LINEBot.Events.MESSAGE, function(replyToken, message) {
        bot.replyText( replyToken, message.getText() ).then((data) => {
            callback();
        }).catch((error) => {
            callback(error);
        });
    });

    const body = event.body;
    const headers = event.headers;
    bot.handleEventRequest( body, headers );
};
