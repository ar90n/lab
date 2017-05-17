import React, { PropTypes } from 'react'
import { config } from 'config'
import SiteRoot from '../components/SiteRoot'
import {SitePost} from '../components/SitePost'
import MathJax from 'react-mathjax'

const MarkdownWrapper = (props) => {
    return (
            <MathJax.Context options={{    extensions: ["tex2jax.js","TeX/AMSmath.js","TeX/AMSsymbols.js", "TeX/noErrors.js","TeX/noUndefined.js"],tex2jax: {inlineMath: [['$','$'], ['\\(','\\)']]}}}>
            <SiteRoot isRoot={false}>
                <SitePost {...props}/>
            </SiteRoot>
        </MathJax.Context>
    );
}

MarkdownWrapper.propTypes = {
    route: React.PropTypes.object,
}

export default MarkdownWrapper
