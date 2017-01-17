import React from 'react'
import Helmet from 'react-helmet'
import { prefixLink } from 'gatsby-helpers'

const BUILD_TIME = new Date().getTime()

module.exports = React.createClass({
    displayName: 'HTML',
    propTypes: {
        body: React.PropTypes.string,
    },
    render() {
        const {body, route} = this.props
        const {title} = Helmet.rewind()
        const font = <link href='https://fonts.googleapis.com/css?family=Roboto:400,400italic,500,700&subset=latin,cyrillic' rel='stylesheet' type='text/css' />
        let css
        let ga
        if (process.env.NODE_ENV === 'production') {
            css = <style dangerouslySetInnerHTML={ {    __html: require('!raw!./public/styles.css')} } />
            ga = <script dangerouslySetInnerHTML={ { __html: `
              (function(i,s,o,g,r,a,m){i['GoogleAnalyticsObject']=r;i[r]=i[r]||function(){
                  (i[r].q=i[r].q||[]).push(arguments)},i[r].l=1*new Date();a=s.createElement(o),
              m=s.getElementsByTagName(o)[0];a.async=1;a.src=g;m.parentNode.insertBefore(a,m)
              })(window,document,'script','https://www.google-analytics.com/analytics.js','ga');

              ga('create', 'UA-90473091-1', 'auto');
              ga('send', 'pageview');
            ` } } />
        }

        return (
            <html lang="en">
            <head>
              <meta charSet="utf-8" />
              <meta httpEquiv="X-UA-Compatible" content="IE=edge" />
              <meta name="viewport" content="width=device-width, initial-scale=1.0 maximum-scale=5.0" />
              { title.toComponent() }
              { font }
              { css }
            </head>
            <body>
              <div id="react-mount" dangerouslySetInnerHTML={ {    __html: this.props.body} } />
              <script src={ prefixLink(`/bundle.js?t=${BUILD_TIME}`) } />
              { ga }
            </body>
            </html>
        )
    },
})
/*
*/
