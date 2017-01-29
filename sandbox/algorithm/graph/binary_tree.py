#!/usr/bin/env python
# -*- coding:utf-8 -*-

class BinaryTreeNode(object):
    def __init__( self, value ):
        self.value = value
        self.left_node = None
        self.right_node = None
        pass

class BinaryTree(object):
    def __init__( self ):
        self.root = None
        pass
    def insert( self, value ):
        if self.root == None:
            self.root = BinaryTreeNode( value )
        else:
            self.__insert( self.root, value )
        pass
    def __insert( self, node, value ):
        if node.value == value:
            return
        elif value < node.value:
            if node.left_node == None:
                node.left_node = BinaryTreeNode( value )
            else:
                self.__insert( node.left_node, value )
        else:
            if node.right_node == None:
                node.right_node = BinaryTreeNode( value )
            else:
                self.__insert( node.right_node, value )
        pass

    def contain( self, value ):
        return self.__contain( self.root, value )
    def __contain( self, node, value ):
        if node == None:
            return False

        if node.value == value:
            return True
        elif value < node.value:
            return self.__contain( node.left_node, value )
        else:
            return self.__contain( node.right_node, value )
        pass
    pass

def main():
    bt = BinaryTree()
    bt.insert( 4 )
    bt.insert( 2 )
    bt.insert( 1 )
    bt.insert( 3 )
    print( bt.contain( 3 ) )
    return

if __name__ == '__main__':
    main()
