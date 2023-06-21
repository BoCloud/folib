import { axios } from "@/utils/request";

const preApi = "/dependency";

// 获取组件数据列表
export function getComponentsList(data) {
  return axios({
    url: `/api/component/page`,
    method: "get",
    params: data,
  });
}

// 获取详情数据
export function getComponentsDetail(data) {
  return axios({
    url: `/api/component/single`,
    method: "get",
    params: data,
  });
}

// 获取头部echarts数据
export function getComponentsCurrent(data) {
  return axios({
    url: `${preApi}/v1/metrics/component/${data}/current`,
    method: "get",
  });
}

// 获取详情数据版本
export function getComponentsVersions(data) {
  return axios({
    url: `${preApi}/v1/project?offset=0&limit=10&excludeInactive=true&name=` + encodeURIComponent(data),
    method: "get",
  });
}

// 项目漏洞
export function getComponentsVulnerabilities(data) {
  return axios({
    url: `${preApi}/v1/metrics/component/${data}/days/90`,
    method: "get",
  });
}

// 获取漏洞数据列表
export function getVulnerabilitiesComponentsList(id, data) {
  return axios({
    url: `${preApi}/v1/vulnerability/component/${id}`,
    method: "get",
    params: data,
  });
}

// 漏洞列表
export function getVulnerability(data) {
  return axios({
    url: `/api/vulnerability/pageByComponent`,
    method: "get",
    params: data,
  });
}

// 制品列表
export function getArtifact(data) {
  return axios({
    url: `/api/component/artifactPage`,
    method: "get",
    params: data,
  });
}

// 制品图谱
export function getArtifactGraph(data) {
  return axios({
    url: `/api/component/artifactGraph`,
    method: "get",
    params: data,
  });
}

// 制品统计
export function getArtifactStatistics(data) {
  return axios({
    url: `/api/component/artifactStatistics`,
    method: "get",
    params: data,
  });
}