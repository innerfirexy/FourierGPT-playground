from collections import deque

class Tree(object):
    def __init__(self, tree_str) -> None:
        assert self.is_valid(tree_str), f"Invalid node string: {tree_str}"
        tree_str = self.clean_outer_parens(tree_str)
        head, children = self.parse_tree_str(tree_str)
        self.head = head
        self.children = children
    
    def is_valid(self, tree_str):
        lp_stack = deque()
        for char in tree_str:
            if char == '(':
                lp_stack.append(char)
            elif char == ')':
                if len(lp_stack) == 0:
                    return False
                lp_stack.pop()
        return True
    
    def clean_outer_parens(self, tree_str):
        """
        Remove outer parentheses and leading space
        Example: ( (S (NP-SBJ The federal government) ... .)), which is typical in PennTreebank .prd files
        """
        if tree_str[0] == '(' and tree_str[-1] == ')' and tree_str[1] == ' ':
            return tree_str[1:-1].strip()
        return tree_str
    
    def parse_tree_str(self, tree_str: str):
        # find the index that splits head and children
        # - if the right child is a leaf node, then it is the first space to the right of head
        # - if the right child is non-leaf, then there could be no space, but the left parenthesis serves as the delimiter
        #   - For example, the first space in `(NP-SBJ (PDT all)(DT the)` could be omitted.
        assert tree_str[0] == '(' and tree_str[-1] == ')'
        str_chopped = tree_str[1:-1]

        # find the first space or left parenthesis
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
            # check if there is gap > 0 between the previous (prev_li, prev_ri) and current (li, ri)
            # if so, then the gap is the children of the current head
            if i == 0:
                if li > 0:
                    candidate = children_str[:li]
                    if candidate.strip() != '': # skip space in children_str
                        children.append(candidate.strip()) # remove any trailing space
                children.append(self.parse_tree_str(children_str[li: ri+1]))
            else:
                _, prev_ri = paren_indices[i-1]
                if li > prev_ri + 1:
                    candidate = children_str[prev_ri+1:li]
                    if candidate.strip() != '': # skip space in children_str
                        children.append(candidate.strip())
                children.append(self.parse_tree_str(children_str[li: ri+1]))
                if i == len(paren_indices) - 1 and ri < len(children_str) - 1:
                    candidate = children_str[ri+1:]
                    if candidate.strip() != '': # skip space in children_str
                        children.append(candidate.strip())
            
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
