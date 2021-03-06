import * as React from 'react';
import {Area as VxArea} from '@vx/shape';
import {LinePath} from '../path';
import {useGraphData} from './useGraphData';

const {useCallback} = React;

export const Area = ({y0, y1, ...remaining}) => {
  const {clipPath, data, x, xScale, yScale} = useGraphData();
  const xFn = useCallback((...d) => xScale(x(...d)), [x, xScale]);
  const y0Fn = useCallback((...d) => yScale(y0(...d)), [y0, yScale]);
  const y1Fn = useCallback((...d) => yScale(y1(...d)), [y1, yScale]);

  return (
    <VxArea
      clipPath={clipPath}
      {...remaining}
      data={data}
      x={xFn}
      y0={y0Fn}
      y1={y1Fn}
    />
  );
};
