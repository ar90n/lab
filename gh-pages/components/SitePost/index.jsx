import React, { PropTypes } from 'react'
import { Link } from 'react-router'
import { prefixLink } from 'gatsby-helpers'
import moment from 'moment'
import { Chip } from 'material-ui'
import { FlatButton } from 'material-ui'
import { Card, CardActions, CardHeader, CardText } from 'material-ui'
import '../../static/css/highlight.css'

const styles = {
    common: {
        marginBottom: '14px'
    },
    category: {
        container: {
            display: 'flex',
            flexDirection: 'row',
            flexWrap: 'wrap'
        },
        label: {
lineHeight: '24px'
        },
        chip: {
            margin: '4px 2px 4px 2px'
        }
    },
date: {
    fontSize: '14px',
},
    title: {
        borderBottom: 'none',
fontSize: 'x-large'
    },
    meta: {
        display: 'flex',
        alignItems: 'center',
        justifyContent: 'space-between',
        flexWrap: 'wrap'
    },
}

const CategoryChips = ({ categories}) => {
categories = Array.isArray(categories) ? categories : [categories]
    return (
        <div style={styles.category.container}>
            {categories.map(( item ) => <Chip key={item} style={styles.category.chip} labelStyle={styles.category.label}>{item}</Chip>)}
        </div>
    )
}

const PostDate = ( {date} ) => {
    return (
        <time dateTime={moment( date ).format( 'MMMM D, YYYY' )} style={styles.date} >
            {moment( date ).format( 'YYYY-MM-DD' )}
        </time >
    )
}

const PostMeta = ( { categories, date } ) => {
    const contents = []
    if(!!categories)
    {
        contents.push( <CategoryChips key="category" categories={categories}/> )
    }
    if(!!date)
    {
        contents.push( <PostDate key="date" date={date} /> )
    }

    return (0 < contents.length) ? (<div style={styles.meta}> {contents} </div>) : null
}

const SiteHeadline = ( {title, description, datePublished, category, path } ) => {
    return (
     <Card style = {styles.common} >
         <CardHeader title={title} titleStyle={styles.title}>
             <PostMeta categories={category} date={datePublished} />
         </CardHeader>
         <CardText >
             { description }
         </CardText >
         <CardActions>
             <Link to={prefixLink( path )}>
                 <FlatButton label="Read" primary={true}/ >
             </Link>
         </CardActions>
     </Card >
 );
}

const SitePost = (props) => {
    const post = props.route.page.data
    return (
     <Card style = {styles.common} >
         <CardHeader title={post.title} titleStyle={styles.title}>
             <PostMeta categories={post.category} date={post.date} />
         </CardHeader>
         <CardText >
             <div dangerouslySetInnerHTML={ {    __html: post.body} } />
         </CardText >
     </Card >
    )
}

export {
    SiteHeadline,
    SitePost
}
