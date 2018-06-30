import React from 'react'
import remark from 'remark'
import remarkReact from 'remark-react'
import remarkMath from 'remark-math'
import Highlight from 'react-highlight'
import { InlineMath, BlockMath } from 'react-katex';
import { Mermaid } from '@components/Mermaid';
import Divider from 'material-ui/Divider';

import './Markdown.css';
import 'katex/dist/katex.min.css';
import 'highlight.js/styles/rainbow.css';
import githubSchema from 'hast-util-sanitize/lib/github.json';

const schema = Object.assign({}, githubSchema, {
  attributes: Object.assign({}, githubSchema.attributes, {
    code: [
      ...(githubSchema.attributes.code || []),
      'className'
    ]
  }),
  tagNames: [
    ...(githubSchema.tagNames),
    'math',
    'imath'
  ]
});

const withMargin = (Type) => {
  const style = {
    marginLeft: '24px',
    marginRight: '24px',
  };
  return ( props ) => {
    const { children, ...leftProps } = props
    return (
      <div style={style}>
        <Type {...leftProps}>
          {children}
        </Type>
      </div>
    );
  };
};

const withDivider = (Type) => {
  const style = {
    margin: '0px',
    paddingTop: '32px',
    paddingBottom: '8px',
  };
  return ( props ) => {
    const { children, ...leftProps } = props
    return (
      <React.Fragment>
        <Type style={style} {...leftProps}>
          {children}
        </Type>
        <Divider />
      </React.Fragment>
    );
  }
};

const p = withMargin('p');
const h1 = withMargin(withDivider('h1'));
const h2 = withMargin(withDivider('h2'));
const h3 = withMargin(withDivider('h3'));
const h4 = withMargin('h4');
const h5 = withMargin('h5');

const code = ( { className, children } ) => {
  if(className == 'language-mermaid') {
    return (
      <Mermaid className={className}>
        {children}
      </Mermaid>
    );
  }
  else {
    const style = {
      marginLeft: '24px',
      marginRight: '24px',
    };
    const style2 = {
      borderRadius: '12px',
    };
    return (
      <Highlight className={className + " round"} style={style2}>
        <div style={style}>
          {children}
        </div>
      </Highlight>
    );
  }
}

const img = ( props ) => {
  props = Object.assign({}, props, {src: `/assets/img/blog/${props.src}`})
  console.log( props );
  return (
    <img {...props} />
  );
}

const math = ( { children } ) => (
  <BlockMath>
    {children[0]}
  </BlockMath>
);

const imath = ({ children }) => (
  <InlineMath>
    {children[0]}
  </InlineMath>
);

const handlers = {
  math: (h, node, parent) => {
    node.data.hName = 'math';
    return h(node, 'math', {});
  },
  inlineMath: (h, node, parent) => {
    node.data.hName = 'imath';
    return h(node, 'imath', {});
  },
};

const processor = remark()
  .use(remarkMath)
  .use(remarkReact, {
    sanitize: schema,
    remarkReactComponents: {
      p,
      h1,
      h2,
      h3,
      h4,
      h5,
      code,
      img,
      math,
      imath
    },
    toHast: { handlers }
  });

interface IProps {
  content: any,
};

const Markdown = ({content}) => (
  <React.Fragment>
    {processor.processSync(content).contents}
  </React.Fragment>
);

export {
  IProps,
  Markdown
};
