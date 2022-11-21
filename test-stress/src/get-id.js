import { initApi } from './api.js';

const api = initApi();

const size = 10000;
var counter = 1;

export function setup() {
  api.setupLogs();
  for (let i = 0; i < size; i++) {
    api.save('Waiting for the summer', ['random']);
  }
};

export default function () {
  counter = 1 + ((counter + 1) % size);
  api.getMessage(counter);
};

