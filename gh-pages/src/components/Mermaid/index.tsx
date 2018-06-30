import * as React from 'react';
import mermaid from 'mermaid';

interface IProps extends React.Props<{}> {
    className?: any;
}

interface IState {
    __html: any;
}

class Mermaid extends React.Component<IProps, IState> {
  constructor(props: IProps) {
    super(props)
    this.state = {
      __html: null
    }

    mermaid.mermaidAPI.initialize({
      startOnLoad: false
    })
  }

  componentDidMount() {
    mermaid.mermaidAPI.render('id', this.props.children[0], __html => {
       this.setState({ __html })
    })
  }

  render() {
    return <div className={this.props.className} dangerouslySetInnerHTML={this.state} />
  }
}

export {
    IState,
    IProps,
    Mermaid,
};
