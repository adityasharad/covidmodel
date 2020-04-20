import * as React from 'react';
import Head from 'next/head';
import {useRouter} from 'next/router';

import {
  Daily,
  EffectiveReproductionNumber,
  FeedbackForm,
  Fitting,
  Hospitalizations,
  ICU,
  Layout,
  ModelInputs,
  OutcomeSummary,
  ParameterTable,
  ProjectedDeaths,
  SEIR,
  TestAndTrace,
} from '../../components';
import {Controls} from '../../components/configured';
import {
  Heading,
  Instruction,
  ListItem,
  Paragraph,
  Section,
  Title,
  UnorderedList,
} from '../../components/content';
import {DistancingGradient} from '../../components/graph';
import {ModelDataProvider, ModelStateProvider} from '../../components/modeling';
import {Suspense, useContentRect} from '../../components/util';
import {stateLabels} from '../../lib/controls';
import {getStatesWithData} from '../../lib/data';
import {initialTargetDate} from '../../lib/date';
import {theme} from '../../styles';

const {useCallback, useRef, useState} = React;

const getCurrentlyCritical = ({currentlyCritical}) => currentlyCritical;

export default function StatePage() {
  const {
    query: {state},
  } = useRouter();
  const [scenario, setScenario] = useState('scenario5');

  const sizeRef = useRef(null);
  const {width} = useContentRect(sizeRef, {width: 896, height: 360});
  const height = width > 600 ? 360 : 256;

  const stateName = stateLabels[state];

  return (
    <Layout>
      <ModelStateProvider
        locationId={state}
        scenarioId={scenario}
        setScenario={setScenario}
      >
        <Head>
          <title>Modeling COVID-19 in {stateName}</title>
          <meta
            name="Description"
            content={`A projection of COVID 19 cases in ${stateName} under various scenarios of social distancing.`}
          />
        </Head>
        <style jsx>{`
          .controls-container {
            position: sticky;
            z-index: 50;
            top: 51px;
            background: ${theme.color.background};
            box-shadow: 0 2px ${theme.color.shadow[0]};
          }
          .controls {
            padding: var(--spacing1) 0;
          }
        `}</style>
        <Suspense fallback={<div />}>
          <svg
            viewBox={`0 0 ${width} 0`}
            style={{
              position: 'absolute',
              pointerEvents: 'none',
              zIndex: -1,
            }}
          >
            <DistancingGradient size={width} />
          </svg>
        </Suspense>
        <div className="flex flex-col justify-center">
          <Section className="margin-top-4 margin-bottom-3">
            <div className="graph-size" ref={sizeRef} />
            <div className="text-jumbo margin-bottom-1">
              <span className="nowrap">Modeling COVID-19</span>{' '}
              <span className="nowrap">in {stateName}</span>
            </div>
            <div className="dek margin-bottom-3">
              Forecasting the impact of the virus using models trained with
              actual social distancing, testing, and fatality data
            </div>

            <Paragraph>
              <strong>“All models are wrong, but some are useful.”</strong> Like
              all models, this model is just one approximation among many. We
              hope it proves useful.
            </Paragraph>

            <Paragraph></Paragraph>

            <Title className="margin-top-5">What are we modeling?</Title>
            <Paragraph>Our model has two primary dimensions:</Paragraph>
            <UnorderedList>
              <ListItem>
                <strong>Location:</strong> We use location to determine the
                demographic data we use, including population, existing data
                about the spread of COVID-19 in the region, and historical
                social distancing levels.
              </ListItem>
              <ListItem>
                <strong>Social distancing scenario:</strong> The social
                distancing scenario models what the people and governments in
                the region might do in the future—how socially distanced will
                they be, and for how long?
              </ListItem>
            </UnorderedList>
            <Instruction>
              <strong>Interact with the model</strong> by selecting a location
              and scenario below. These controls will remain docked to the top
              of the screen.
            </Instruction>
          </Section>
          <div className="controls-container">
            <Section>
              <div className="controls">
                <Controls />
              </div>
            </Section>
          </div>
          <Section className="margin-top-3">
            <ModelInputs width={width} height={192} />
            <TestAndTrace width={width} height={height} />
            <SEIR width={width} height={height} />
            <Fitting width={width} height={height} />
            <Daily width={width} height={height} />
            <EffectiveReproductionNumber width={width} height={192} />
            {/* <ProjectedDeaths width={width} height={height} /> */}
            <ParameterTable />
          </Section>
          <Section style={{marginTop: '300px'}}>
            <div className="text-jumbo">Work in progress</div>
            <div className="dek margin-top-1">
              These sections are being converted to match the format&nbsp;above.
            </div>
            <Hospitalizations width={width} height={height} />
            <ICU width={width} height={height} />
          </Section>
        </div>
      </ModelStateProvider>
      <FeedbackForm />
    </Layout>
  );
}

export const getStaticProps = ({params: {state}}) => {
  return {
    props: {},
  };
};

export const getStaticPaths = (_ctx) => {
  return {
    paths: getStatesWithData().map((state) => ({
      params: {state},
    })),
    fallback: false,
  };
};
