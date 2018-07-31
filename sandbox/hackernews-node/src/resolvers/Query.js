async function feed(parent, args, context, info) {
    const where = args.filter
      ? {
          OR: [
              { url_contains: args.filter },
              { description_contains: args.filter },
          ],
        }
      : {}
    const queryLinks = await context.db.query.links(
        { where, skip: args.skip, first: args.first, orderBy: args.orderBy },
        `{ id }`
    )

    const countSelectionSet = `
        {
            aggregate {
                count
            }
        }
    `

    const linkConnection = await context.db.query.linksConnection({}, countSelectionSet)

    return {
        count: linkConnection.aggregate.count,
        linkIds: queryLinks.map(link => link.id)
    }
}



module.exports = {
    feed,
}