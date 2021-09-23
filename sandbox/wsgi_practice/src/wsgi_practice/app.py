from wsgiref.simple_server import make_server

def application(environ, start_response):
   response_body = [
       f"{key}: {value}" for key, value in sorted(environ.items())
   ]
   response_body = "\n".join(response_body)

   status = "200 OK"

   response_headers = [
      ("Content-type", "text/plain")
   ]

   start_response(status, response_headers)

   return [response_body.encode("utf-8")]

if __name__ == "__main__":
   server = make_server("localhost", 8000, app=application)
   server.serve_forever()
