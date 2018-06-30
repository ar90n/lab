import React from 'react'
import { StyleSheet, css } from 'aphrodite'
import { RouteHandler, Link } from 'react-router'
import { prefixLink } from 'gatsby-helpers'
import { config } from 'config'
import FontaAwesome from 'react-fontawesome'
import * as colors from 'material-ui/styles/colors';

const styles = StyleSheet.create({
    common: {
        marginLeft: '16px'
    },
    list: {
        listStyle: 'none',
        padding: '0',
        margin: '10px 0',
        clear: 'fix-legacy'
    },
    container: {
        float: 'left',
        marginRight: '5px',
        textAlign: 'center',
        height: '24px',
        width: '24px',
        ':hover': {
            background: colors.grey300
        }
    },
    icon: {
        color: '#606060',
        fontSize: '21px',
        lineHeight: '24px',
        ':hover': {
            color: '#444',
        }
    }
})

const SiteLinks = ( props ) => {
    return (
        <div className={css(styles.common)}>
            <ul className={css(styles.list)}>
                <li className={css(styles.container)}>
                    <a href={config.siteTwitterUrl}>
                      <FontaAwesome name='fab fa-twitter' className={css(styles.icon)}/>
                    </a>
                </li>
                <li className={css(styles.container)}>
                    <a href={config.siteGithubUrl}>
                      <FontaAwesome name='fab fa-github-alt' className={css(styles.icon)}/>
                    </a>
                </li>
                <li className={css(styles.container)}>
                    <a href={`mailto:${config.siteEmailUrl}`}>
                      <FontaAwesome name='fab fa-envelope-o' className={css(styles.icon)}/>
                    </a>
                </li>
            </ul>
        </div>
    );
}

export default SiteLinks
