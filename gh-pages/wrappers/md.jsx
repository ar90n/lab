import React, { PropTypes } from 'react'
import { config } from 'config'
import SiteRoot from '../components/SiteRoot'
import {SitePost} from '../components/SitePost'

const MarkdownWrapper = (props) => {
    return (
        <SiteRoot isRoot={false}>
            <SitePost {...props}/>
        </SiteRoot>
    );
}

MarkdownWrapper.propTypes = {
    route: React.PropTypes.object,
}

export default MarkdownWrapper
