import Auth from '@aws-amplify/auth';
import React, { useState, useLayoutEffect } from 'react';
import { Button } from 'aws-amplify-react';
import { ConsoleLogger as Logger } from '@aws-amplify/core';
import './App.css';

import config from './aws-exports.js';
Auth.configure(config);


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
  const [isAuthenticated, setIsAuthenticated] = useState(null)

  const signUp = async () => {
    const info = {
      username: makeUniqueString(32),
      password: makeUniqueString(32),
    };
    logger.info(JSON.stringify(info));

    await Auth.signUp(info)
    await Auth.signIn(info)
    setIsAuthenticated(true)
  }

  const signOut = async () => {
    await Auth.signOut()
    setIsAuthenticated(false)
  }


  useLayoutEffect(() => {
    Auth.currentAuthenticatedUser()
      .then(() => setIsAuthenticated(true))
      .catch(() => setIsAuthenticated(false))
  })

  return [isAuthenticated, signUp, signOut]
}

const App = () => {
  const [isAuthenticated, signUp, signOut] = useAuth();

  const signOutButton = <Button onClick={signOut}> signOut </Button> 
  const signUpButton = <Button onClick={signUp}> signUp </Button>
  return (
    <div className="App">
      <header className="App-header">
        { isAuthenticated ? signOutButton : signUpButton }
      </header>
    </div>
  );
}

export default App;
