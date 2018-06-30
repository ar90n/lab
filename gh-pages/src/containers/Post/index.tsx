import React from 'react'
import { withRouteData, Link } from 'react-static'

import {PostContent} from './PostContent'
import PostNavi from './PostNavi'

export default withRouteData((props) => {
  return (
    <React.Fragment>
      <PostContent {...props} />
      <PostNavi prev={props.prev} next={props.next} />
    </React.Fragment>
  );
})
