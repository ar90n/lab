#!/usr/bin/env node

import RSVP from 'rsvp'
import prompt from 'prompt'
import fs from 'fs-extra'
import https from 'https'
import client from 'cheerio-httpcli'
import program from 'commander'
import * as url2 from 'url'

program
    .option('-u, --user [user]', 'user' )
    .option('-p, --password [password]', 'password' )
    .parse( process.argv );

let properties = []
if( !program.user )
{
    properties.push( {
        name : 'mail',
    });
}

if( !program.password )
{
    properties.push( {
        name : 'password',
        hidden : true
    });
}

prompt.start();

const url_8122 = "https://8122.jp";

prompt.get( properties, (err,result) => {
    const mail = program.user ? program.user : result.mail;
    const password = program.password ? program.password : result.password;
    client.fetch( url_8122, ( err, $, res, body ) => {
        let form = $('div.p-globalBeforeLoginHeader_loginForm').find('form');
        form.field({
            'mailaddress' : mail,
            'passwd' : password
        });

        form.find('button[type=submit]').click( ( err, $, res, body ) => {
            const cookies = res.cookies;
            const event_nos = $('div#content').find('a.p-eventList_listItemLink').map( (i,elem) => {
                const strs = $(elem).attr('href').split('=');
                return strs[strs.length-1];
            }).get();

            RSVP.all( event_nos.map( (event_no) => {
                const api_url = `${url_8122}/?action_Api_FastViewer_PhotoList=false&eventno=${event_no}`;
                return client.fetch( api_url );
            })).then((res) => {
                let event_info = [];
                res.forEach( (v,i)  => {
                    if(v.response.statusCode === 200){
                        const eventname = JSON.parse(v.body).message.eventname;
                        event_info.push({
                            event_no: event_nos[i],
                            eventname,
                        });

                        const item_str = `${i}. ${eventname}`
                        console.log( item_str );
                    }
                });

                let fetch_event_nos_property = [ { name : 'Choose number' } ];
                prompt.get( fetch_event_nos_property, (err,result) => {
                    const choose_number = result['Choose number'];
                    const choose_event = event_info[choose_number];

                    const url = `${url_8122}/?action_user_FastViewer=t&eventno=${choose_event.event_no}`;
                    client.fetch( url, ( err, $, res, body ) => {
                        const category_nos = $('div.p-categoryList').find('li').map( (i,elem) => $(elem).attr('data-categoryno'));

                        const category_info = category_nos.map((i,category_no) => {
                            const api_url = `${url_8122}/?action_Api_FastViewer_PhotoList=true&eventno=${choose_event.event_no}&categoryno=${category_no}`;

                            let res_json = JSON.parse(client.fetchSync( api_url ).body).message;
                            const result = {
                                photos: res_json.photos,
                                major: res_json.currentDaiCategoryName,
                                minor: res_json.currentShouCategoryName
                            };

                            let offset = 0;
                            while(res_json.pager.hasnext)
                            {
                                offset += 50;
                                const api_url = `${url_8122}/?action_Api_FastViewer_PhotoList=true&eventno=${choose_event.event_no}&categoryno=${category_no}&offset=${offset}`;
                                res_json = JSON.parse(client.fetchSync( api_url ).body).message;
                                result.photos = result.photos.concat(res_json.photos);
                            }

                            return result;
                        });
                        console.log( category_info );

                        const save_path = './' + choose_event.eventname ;
                        category_info.map((i,info) => {
                            const dir_path = `${save_path}/${info.major}/${info.minor}`;
                            fs.mkdirs( dir_path, (err) => {
                                const get_image = (i) => {
                                    if( i < info.photos.length )
                                    {
                                        const obj = info.photos[i];
                                        const image_name = `${obj.n}-${obj.p}.jpg`;
                                        const image_path = `${dir_path}/${image_name}`;
                                        let file = fs.createWriteStream( image_path );
                                        const urlObj = new url2.URL( obj.m );
                                        let cookies_str = Object.keys(cookies).reduce( (acc,key) => acc.concat( key + '=' + cookies[key] + ';' ), '' );
                                        cookies_str = cookies_str.substr( 0, cookies_str.length - 1 );
                                        const options = {
                                                hostname: urlObj.hostname,
                                                path: urlObj.pathname + urlObj.search,
                                                headers: {
                                                    'Cookie': cookies_str,
                                                }
                                        };
                                        https.get( options, function(response) {
                                            response.pipe(file);
                                            console.log( 'donwloaded ' + image_path );
                                            get_image(i + 1 );
                                        });
                                    }
                                };
                                get_image(0);
                            });
                        });
                    });
                });
            });
        });
    });
});
