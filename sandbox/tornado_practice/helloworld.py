#! /usr/bin/env python3
# -*- coding: utf-8 -*-

import tornado.ioloop
import tornado.web


class MainHandler(tornado.web.RequestHandler):

    def get(self):
        self.write("Hello, world")


def make_app():
    return tornado.web.Application([
        (r'/', MainHandler),
    ])


def main():
    app = make_app()
    app.listen(8888)
    tornado.ioloop.IOLoop.current().start()


if __name__ == '__main__':
    main()
