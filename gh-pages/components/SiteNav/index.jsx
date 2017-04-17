// @flow
import React from 'react'
import { Link } from 'react-router'
import { prefixLink } from 'gatsby-helpers'
import { List, ListItem, makeSelectable } from 'material-ui'
import {ActionDescription, ActionLockOutline } from 'material-ui/svg-icons'

const SelectableList = makeSelectable( List );

const SiteNav = ( props ) => {
    return (
        <SelectableList onChange={props.onChange}>
            <ListItem primaryText="Articles" leftIcon={<ActionDescription />} value={prefixLink( '/' )} />
            <ListItem primaryText="Privacy policy" leftIcon={<ActionLockOutline />} value={prefixLink( '/privacy-policy/' )}/>
        </SelectableList>
    )
}

export default SiteNav
