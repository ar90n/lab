import React from 'react'
import { prefixLink } from 'gatsby-helpers'
import { List, ListItem, makeSelectable } from 'material-ui'

const SelectableList = makeSelectable( List );

const SiteNav = (props) => {
  return (
    <SelectableList onChange={props.onChange} >
      <ListItem primaryText="Articles" value={prefixLink('/')} />
      <ListItem primaryText="PrivacyPolicy" value={prefixLink('/privacy-policy/')} />
    </SelectableList>
  )
}

SiteNav.propTypes = {
  onChange: React.PropTypes.object,
}

export default SiteNav
