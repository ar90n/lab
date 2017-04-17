import React, { PropTypes } from 'react'
import { Motion, spring } from 'react-motion'

const SiteContents = ({ style, children }) => {
    const motionStyle = {
        paddingLeft: spring( style.paddingLeft )
    }
    return (
        <Motion style={motionStyle}>
            {( transStyle ) => {
                const contentsStyle = Object.assign( {}, style, transStyle )
                return <div style={contentsStyle}>
                    {children}
                </div>
            }}
        </Motion>
    )
}

export default SiteContents
