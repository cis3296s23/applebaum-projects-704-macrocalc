from flask import Flask, render_template, redirect, url_for, session, request
from google.oauth2 import id_token
from google.auth.transport.requests import Request
import json
import requests

app = Flask(__name__)
# Read the client secret file and extract the secret key
with open('client_secret.json', 'r') as f:
    data = json.load(f)
app.secret_key = data['web']['client_secret']
CLIENT_ID = data['web']['client_id']

#print("aaaaaaaaaaaaaaaaa")
#print(json.dumps(data))

@app.route('/login')
def login():
    session['state'] = 'random-state-value'
    return redirect(f'https://accounts.google.com/o/oauth2/v2/auth'
                    f'?response_type=code&client_id={CLIENT_ID}'
                    f'&redirect_uri=http://localhost:5000/login/callback'
                    f'&scope=openid%20email%20profile'
                    f'&state={session["state"]}')
                    
@app.route('/logout')
def logout():
    session.clear()
    return redirect(url_for('login'))

@app.route('/login/callback')
def callback():
    state = session.pop('state', None)
    if state is None or state != request.args.get('state'):
        return 'Invalid state parameter'
    code = request.args.get('code')
    try:
        # Exchange the authorization code for a token
        token = requests.post('https://oauth2.googleapis.com/token', data={
            'code': code,
            'client_id': CLIENT_ID,
            'client_secret': app.secret_key,
            'redirect_uri': 'http://localhost:5000/login/callback',
            'grant_type': 'authorization_code'
        }).json()
        
        # Verify the token and extract the user info
        idinfo = id_token.verify_oauth2_token(token['id_token'], Request(), CLIENT_ID)
        session['email'] = idinfo['email']
        session['name'] = idinfo['name']
        return redirect(url_for('profile'))
    except ValueError:
        return 'Invalid token'
      

@app.route('/profile')
def profile():
    if 'email' in session:
        return f'Hello, {session["name"]} ({session["email"]})'
    else:
        return redirect(url_for('login'))


if __name__ == '__main__':
    app.run(debug=True)
