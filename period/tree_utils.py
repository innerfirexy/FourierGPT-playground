from collections import deque

class Tree(object):
    def __init__(self, node_str) -> None:
        assert self.is_valid(node_str), f"Invalid node string: {node_str}"
        node_str = self.clean_outer_parens(node_str)
        head, children = self.parse_node_str(node_str)
        self.head = head
        self.children = children
    
    def is_valid(self, node_str):
        lp_stack = deque()
        for char in node_str:
            if char == '(':
                lp_stack.append(char)
            elif char == ')':
                if len(lp_stack) == 0:
                    return False
                lp_stack.pop()
        return True
    
    def clean_outer_parens(self, node_str):
        """
        Remove outer parentheses and leading space
        Example: ( (S (NP-SBJ The federal government) ... .)), which is typical in PennTreebank .prd files
        """
        if node_str[0] == '(' and node_str[-1] == ')' and node_str[1] == ' ':
            return node_str[2:-1]
        return node_str
    
    def parse_node_str(self, node_str: str):
        # find the index that splits head and children
        # - if the right child is a leaf node, then it is the first space to the right of head
        # - if the right child is non-leaf, then there could be no space, but the left parenthesis serves as the delimiter
        #   - For example, the first space in `(NP-SBJ (PDT all)(DT the)` could be omitted.
        str_chopped = node_str[1:-1]
        for i, char in enumerate(str_chopped):
            if char == ' ':
                head = str_chopped[:i]
                children_str = str_chopped[i+1:]
                break
            elif char == '(':
                head = str_chopped[:i]
                children_str = str_chopped[i:]
                break

        # boundary case: the right child is a leaf node
        if '(' not in children_str:
            return head, [children_str]
        
        lp_stack = deque()
        paren_indices = []
        for i, char in enumerate(children_str):
            if char == '(':
                lp_stack.append(i)
            elif char == ')':
                if len(lp_stack) > 1:
                    lp_stack.pop()
                else:
                    paren_indices.append((lp_stack.pop(), i))
        
        # recursive calls
        children = []
        for i, (li, ri) in enumerate(paren_indices):
            # check if there is gap >0 between the previous (prev_li, prev_ri) and current (li, ri)
            # if so, then the gap is the children of the current head
            if i == 0:
                if li > 0:
                    children.append(children_str[:li])
                children.append(self.parse_node_str(children_str[li: ri+1]))
            else:
                _, prev_ri = paren_indices[i-1]
                if li > prev_ri + 1:
                    children.append(children_str[prev_ri+1:li])
                children.append(self.parse_node_str(children_str[li: ri+1]))
                if i == len(paren_indices) - 1 and ri < len(children_str) - 1:
                    children.append(children_str[ri+1:])
            
        return head, children
    
    def __repr__(self) -> str:
        prefix = '(' + self.head + ' '
        postfix = ')'
        if isinstance(self.children, list):
            nodes_str = ''.join([str(node) for node in self.children])
            return prefix + nodes_str + postfix
        else:
            return prefix + self.children + postfix
    
    def DFS_visit(self):
        pass

    def leaves(self):
        stack = deque()
        output = []
        for child in self.children:
            stack.append(child)
        while len(stack) > 0:
            node = stack.pop()
            if isinstance(node, str):
                output.append(node)
            elif isinstance(node, tuple):
                children = node[1]
                for child in children:
                    stack.append(child)
        return list(reversed(output))
