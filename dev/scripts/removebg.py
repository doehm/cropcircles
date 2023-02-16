import requests

def call_remove_bg(api_key, image, to):
  response = requests.post(
      'https://api.remove.bg/v1.0/removebg',
      files={'image_file': open(image, 'rb')},
      data={'size': 'auto'},
      headers={'X-Api-Key': api_key},
  )
  if response.status_code == requests.codes.ok:
      with open(to, 'wb') as out:
          out.write(response.content)
  else:
      print("Error:", response.status_code, response.text)
  
for k in range(0,len(r.images)):
  print(r.images)
  call_remove_bg(api_key = r.api_key, image = r.images, to = r.to)
