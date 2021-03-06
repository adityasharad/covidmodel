import * as React from 'react';
import css from 'styled-jsx/css';
import {breakpoint, theme} from '../../styles';

const styles = css`
  span {
    font-weight: 500;
    font-family: ${theme.font.family.mono};
    text-transform: uppercase;
    padding: 0 4px;
    margin-left: -2px;
    margin-right: -2px;
  }
  span.label-gray {
    color: ${theme.color.gray.muted};
    background: ${theme.color.gray[0]};
  }
  span.label-yellow {
    color: ${theme.color.yellow.muted};
    background: ${theme.color.yellow[0]};
  }
  span.label-purple {
    color: ${theme.color.purple.muted};
    background: ${theme.color.purple[0]};
  }
  span.label-blue {
    color: ${theme.color.blue.muted};
    background: ${theme.color.blue[0]};
  }
  @media (prefers-color-scheme: dark) {
    span.label-yellow {
      background: ${theme.color.yellow[1]};
    }
  }
`;

const getHighlightClass = (color) => {
  switch (color) {
    case 'blue':
      return 'label-blue';
    case 'purple':
      return 'label-purple';
    case 'yellow':
      return 'label-yellow';
    case 'gray':
    default:
      return 'label-gray';
  }
};

export const Label = ({children, className, color, ...remaining}) => {
  return (
    <span
      className={[getHighlightClass(color), className]
        .filter(Boolean)
        .join(' ')}
      {...remaining}
    >
      <style jsx>{styles}</style>
      {children}
    </span>
  );
};
