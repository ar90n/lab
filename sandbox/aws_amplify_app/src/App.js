import Auth from '@aws-amplify/auth';
import Storage from '@aws-amplify/storage';
import API, { graphqlOperation } from '@aws-amplify/api';
import React, { useState, useLayoutEffect, useEffect } from 'react';
import { Button, S3Image } from 'aws-amplify-react';
import { ConsoleLogger as Logger } from '@aws-amplify/core';
import './App.css';
import { listRequests } from './graphql/queries';

import config from './aws-exports.js';
Auth.configure(config);
Storage.configure(config);
API.configure(config);


const logger = new Logger('App');

const makeUniqueString = (length) => {
   let result = '';
   const characters = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789';
   for ( let i = 0; i < length; i++ ) {
      result += characters.charAt(Math.floor(Math.random() * characters.length));
   }
   return result;
}

const useAuth = () => {
  const [userId, setIsAuthenticated] = useState(null)

  const signUp = async () => {
    const info = {
      username: makeUniqueString(32),
      password: makeUniqueString(32),
    };
    logger.info(JSON.stringify(info));

    await Auth.signUp(info)
    const user = await Auth.signIn(info)
    setIsAuthenticated(user.username)
  }

  const signOut = async () => {
    await Auth.signOut()
    setIsAuthenticated(null)
  }


  useLayoutEffect(() => {
    Auth.currentAuthenticatedUser()
      .then((user) => setIsAuthenticated(user.username))
      .catch(() => setIsAuthenticated(null))
  })

  return [userId, signUp, signOut]
}

const putToStorage = (content) => {
  if(!content) {
    return
  }

  const level = 'private'
  const contentType =  content.type
  const key = makeUniqueString(32)
  const metadata = { hoge : 'abc' }
  Storage.put(key, content, { level, contentType, metadata });
}

const ImagePicker = (props) => {
  const styles = {
    container: {
      display: 'inline-block',
      position: 'relative',
      height: '100px',
      width: '400px'
    },
    label: {
      position: 'absolute',
      left: 0,
      top: 0,
      bottom: 0,
      width: '100%',
      display: 'flex',
      alignItems: 'center',
      justifyContent: 'center',
      background: '#ccc',
      border: '3px dotted #bebebe',
      borderRadius: '10px',
    },
    input: {
      width: '100%',
      display: 'inline-block',
      position: 'absolute',
      cursor: 'pointer',
      left: 0,
      opacity: 0,
      top: 0,
      bottom: 0
    }
  }

  const { onChange } = props
  return (
    <div style={styles.container}>
      <div style={styles.label}>
        { props.children }
      </div>
      <input
          type="file"
          accept="image/*"
          capture="environment"
          style={styles.input}
          onChange={ onChange }
      />
    </div>
  )
}

const App = () => {
  const [userId, signUp, signOut] = useAuth();
  const [requests, setRequests] = useState([]);

  const signOutButton = <Button onClick={signOut}> signOut </Button> 
  const signUpButton = <Button onClick={signUp}> signUp </Button>

  useEffect(() => {
    (async () => {
      const requestData = await API.graphql(graphqlOperation(listRequests))
      setRequests(requestData.data.listRequests.items)
    })()
  })

  return (
    <div className="App">
      <header className="App-header">
        <div>
          { !!userId ? userId : null }
        </div>
        <div>
          { !!userId ? signOutButton : signUpButton }
        </div>
        <div>
          <ImagePicker onChange={(e) => putToStorage(e.target.files[0])}>
            upload
          </ ImagePicker>
        </div>
        <div>
          { !!userId ? requests.map(({ id, bucket, key }) => <S3Image level='private' theme={{photoImg: {width:"100"}}} imgKey={key.split('/')[2]} />) : null }
        </div>
      </header>
    </div>
  );
}

export default App;
