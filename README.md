# real-world-conduit

add description of real-world-conduit here

Just go ahead to this [write up](siskam.link/2018-07-02-thinkster-s-realworld---backend.html).

### Backend Endpoints
```
Endpoint                         | Method | What                         | Auth?    | Req. Body         | Return
---------------------------------|--------|------------------------------|----------|-------------------|----------
/api/user                        | GET    | Current user's information   | Yes      |                   | user.json
/api/user                        | PUT    | Update user's information    | Yes      | requpdtuser.json  | user.json
/api/users                       | POST   | Registration                 | No       | reqregister.json  | user.json
/api/users/login                 | POST   | Login                        | No       | reqlogin.json     | user.json
/api/profiles/:username          | GET    | :username's profile          | Opt      |                   | profile.json
/api/profiles/:username/follow   | DELETE | Follow :username             | Yes      |                   | profile.json
/api/profiles/:username/follow   | POST   | Follow :username             | Yes      |                   | profile.json
/api/articles                    | GET    | Latest articles              | Opt      |                   | articles.json
                                 |        | Params:                      |          |                   |
                                 |        |   - tag: Text                |          |                   |
                                 |        |   - author: Text             |          |                   |
                                 |        |   - favorited: Text          |          |                   |
                                 |        |   - limit: Int (20)          |          |                   |
                                 |        |   - offset: Int (0)          |          |                   |
/api/articles                    | POST   | Create article               | Yes      | reqcreartic.json  | article.json
/api/articles/:slug              | DELETE | Delete article :slug         | Yes      |                   |
/api/articles/:slug              | GET    | Article with slug :slug      | No       |                   | article.json
/api/articles/:slug              | PUT    | Update article :slug         | Yes      | requpdtartic.json | article.json
/api/articles/:slug/comments     | GET    | Comment on article :slug     | Opt      |                   | comments.json
/api/articles/:slug/comments     | POST   | Comment on article :slug     | Yes      | reqcomment.json   | comment.json
/api/articles/:slug/comments/:id | DELETE | Comment on article :slug :id | Yes      |                   |
/api/articles/:slug/favorite     | DELETE | Unfavorite article :slug     | Yes      |                   | article.json 
/api/articles/:slug/favorite     | POST   | Favorite article :slug       | Yes      |                   | article.json
/api/articles/feed               | GET    | Latest articles by followed  | Yes      |                   | articles.json
                                 |        |   - limit: Int (20)          |          |                   |
                                 |        |   - offset: Int (0)          |          |                   |
/api/tags                        | GET    | List of tags                 | No       |                   | tags.json
```
#### Example reqregister.json
```
{
  "user":{
    "username": "Jacob",
    "email": "jake@jake.jake",
    "password": "jakejake"
  }
}
```
#### Example reqlogin.json
```
{
  "user":{
    "email": "jake@jake.jake",
    "password": "jakejake"
  }
}
```
#### Example requpdtuser.json
```
{
  "user":{
    "email": "jake@jake.jake", // nullable
    "bio": "I like to skateboard", // nullable
    "image": "https://i.stack.imgur.com/xHWG8.jpg", // nullable
    "username": null, // nullable
    "password": null // nullable
  }
}
```
#### Example reqcreartic.json
```
{
  "article": {
    "title": "How to train your dragon",
    "description": "Ever wonder how?",
    "body": "You have to believe",
    "tagList": ["reactjs", "angularjs", "dragons"] // nullable
  }
}
```
#### Example requpdtartic.json
```
{
"article": {
    "title": "How to train your dragon", // nullable
    "description": "Ever wonder how?", // nullable
    "body": "You have to believe", // nullable
  }
}
```
#### Example reqcomment.json
```
{
  "comment": {
    "body": "His name was my name too."
  }
}
```
#### Example user.json
```
{
  "user": {
    "email": "jake@jake.jake",
    "token": "jwt.token.here",
    "username": "jake",
    "bio": "I work at statefarm", // nullable
    "image": null
  }
}
```
### Example profile.json
```
{
  "profile": {
    "username": "jake",
    "bio": "I work at statefarm", // nullable
    "image": "https://static.productionready.io/images/smiley-cyrus.jpg", // nullable
    "following": false
  }
}
```
### Example article.json
```
{
  "article": {
    "slug": "how-to-train-your-dragon",
    "title": "How to train your dragon",
    "description": "Ever wonder how?",
    "body": "It takes a Jacobian",
    "tagList": ["dragons", "training"], // nullable
    "createdAt": "2016-02-18T03:22:56.637Z",
    "updatedAt": "2016-02-18T03:48:35.824Z", // nullable
    "favorited": false,
    "favoritesCount": 0,
    "author": {
      "username": "jake",
      "bio": "I work at statefarm",
      "image": "https://i.stack.imgur.com/xHWG8.jpg",
      "following": false
    }
  }
}
```
### Example articles.json
```
{
  "articles":[{
    "slug": "how-to-train-your-dragon",
    "title": "How to train your dragon",
    "description": "Ever wonder how?",
    "body": "It takes a Jacobian",
    "tagList": ["dragons", "training"],
    "createdAt": "2016-02-18T03:22:56.637Z",
    "updatedAt": "2016-02-18T03:48:35.824Z",
    "favorited": false,
    "favoritesCount": 0,
    "author": {
      "username": "jake",
      "bio": "I work at statefarm",
      "image": "https://i.stack.imgur.com/xHWG8.jpg",
      "following": false
    }
  }, {
    "slug": "how-to-train-your-dragon-2",
    "title": "How to train your dragon 2",
    "description": "So toothless",
    "body": "It a dragon",
    "tagList": ["dragons", "training"],
    "createdAt": "2016-02-18T03:22:56.637Z",
    "updatedAt": "2016-02-18T03:48:35.824Z",
    "favorited": false,
    "favoritesCount": 0,
    "author": {
      "username": "jake",
      "bio": "I work at statefarm",
      "image": "https://i.stack.imgur.com/xHWG8.jpg",
      "following": false
    }
  }],
  "articlesCount": 2
}
```
### Example comment.json
```
{
  "comment": {
    "id": 1,
    "createdAt": "2016-02-18T03:22:56.637Z",
    "updatedAt": "2016-02-18T03:22:56.637Z",
    "body": "It takes a Jacobian",
    "author": {
      "username": "jake",
      "bio": "I work at statefarm",
      "image": "https://i.stack.imgur.com/xHWG8.jpg",
      "following": false
    }
  }
}
```
### Example comments.json
```
{
  "comments": [{
    "id": 1,
    "createdAt": "2016-02-18T03:22:56.637Z",
    "updatedAt": "2016-02-18T03:22:56.637Z",
    "body": "It takes a Jacobian",
    "author": {
      "username": "jake",
      "bio": "I work at statefarm",
      "image": "https://i.stack.imgur.com/xHWG8.jpg",
      "following": false
    }
  }]
}
```
### Example tags.json
```
{
  "tags": [
    "reactjs",
    "angularjs"
  ]
}
```
