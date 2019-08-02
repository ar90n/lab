// eslint-disable
// this is an auto generated file. This will be overwritten

export const getRequest = `query GetRequest($id: ID!) {
  getRequest(id: $id) {
    id
    bucket
    key
    description
  }
}
`;
export const listRequests = `query ListRequests(
  $filter: ModelRequestFilterInput
  $limit: Int
  $nextToken: String
) {
  listRequests(filter: $filter, limit: $limit, nextToken: $nextToken) {
    items {
      id
      bucket
      key
      description
    }
    nextToken
  }
}
`;
