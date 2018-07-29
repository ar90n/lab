const { GraphQLServer } = require('graphql-yoga');

let links = [{
  id: 'link-0',
  url: 'www.howtographql.com',
  description: 'Fullstack tutorial for GraphQL'
}]

let idCount = links.length
const resolvers = {
  Query: {
    info: () => `This is the API of Hackernews Clone`,
    feed: () => links,
    link: (_, args) => {
      id = args.id
      return links.find((e) => e.id == id)
    }
  },
  Mutation: {
    post: (root, args) => {
      const link = {
        id: `link-${idCount++}`,
        description: args.description,
        url: args.url,
      }
      links.push(link)
      return link
    },
    updateLink: (root, args) => {
      const link = links.find((e) => e.id == args.id)
      if(link == undefined) {
        return link
      }

      link.description = args.description || link.description
      link.url = args.url || link.url
      return link
    },
    deleteLink: (root, args) => {
      const link = links.find((e) => e.id == args.id)
      if(link != undefined) {
        const i = links.indexOf(link)
        links.splice(i, 1)
      }
      return link
    }
  }
}

const server = new GraphQLServer({
  typeDefs: './src/schema.graphql',
  resolvers,
})
server.start(() => console.log(`Server is running on http://localhost:4000`))
