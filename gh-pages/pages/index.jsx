import React, { PropTypes } from 'react'
import sortBy from 'lodash/sortBy'
import access from 'safe-access'
import { config } from 'config'
import {SiteHeadline} from '../components/SitePost'
import SiteRoot from '../components/SiteRoot'

const SiteIndex = ( props ) => {
    const pageLinks = sortBy(props.route.pages, page => access( page, 'data.date' ))
        .reverse( )
        .filter( ( page ) => access( page, 'file.ext' ) === 'md' && access( page, 'data.layout' ) === 'post' )
        .map(( page ) => {
            const title = access( page, 'data.title' ) || page.path
            const description = access( page, 'data.description' )
            const datePublished = access( page, 'data.date' )
            const category = access( page, 'data.category' )
            const path = page.path
            return <SiteHeadline title={title} description={description} datePublished={datePublished} category={category} path={path}/>
    })

    return (
        <SiteRoot isRoot={true}>
            {pageLinks}
        </ SiteRoot>
    )
}

export default SiteIndex
