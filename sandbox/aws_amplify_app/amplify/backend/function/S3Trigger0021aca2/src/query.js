module.exports = {
    mutation: `mutation createRequest($input: CreateRequestInput!) {
      createRequest(input: $input) {
        id
        bucket
        key
        description
      }
    }
    `
}
