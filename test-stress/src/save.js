import { initApi } from './api.js';

const api = initApi();

export function setup () {
  api.setupLogs();
};

export default function () {
  api.save('Waiting for the summer', ['random']);
}
