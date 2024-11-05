import { axios } from "@/utils/request";

const preApi = "/dependency";

// 获取项目数据列表
export function getProjectsList(data) {
  return axios({
    url: `${preApi}/v1/project`,
    method: "get",
    params: data,
  });
}

// 获取详情头部数据
export function getProjectsHeaderDetail(data) {
  return axios({
    url: `${preApi}/v1/project/${data}`,
    method: "get",
  });
}

// 获取组件详情头部数据
export function getComponentHeaderDetail(data) {
  return axios({
    url: `${preApi}/v1/component/${data}`,
    method: "get",
  });
}

// 获取头部echarts数据
export function getProjectsCurrent(data) {
  return axios({
    url: `${preApi}/v1/metrics/project/${data}/current`,
    method: "get",
  });
}

// 获取头部echarts数据
export function getComponentCurrent(data) {
  return axios({
    url: `${preApi}/v1/metrics/component/${data}/current`,
    method: "get",
  });
}

// 获取详情数据版本
export function getProjectVersions(data,) {
  return axios({
    url: `${preApi}/v1/project?offset=0&limit=10&excludeInactive=true&name=` + encodeURIComponent(data),
    method: "get",
  });
}

// 项目漏洞
export function getProjectsVulnerabilities(data) {
  return axios({
    url: `${preApi}/v1/metrics/project/${data}/days/90`,
    method: "get",
  });
}

// 项目漏洞
export function getComponentVulnerabilities(data) {
  return axios({
    url: `${preApi}/v1/metrics/component/${data}/days/90`,
    method: "get",
  });
}

// 项目组件
export function getProjectsComponents(id, data) {
  return axios({
    url: `${preApi}/v1/component/project/${id}`,
    method: "get",
    params: data,
  });
}

// 项目组件
// export function getProjectsComponents(data) {
//   return axios({
//     url: `/api/component/page`,
//     method: "get",
//     params: data,
//   });
// }

export function getProjectsComponentsByArtifact(data) {
  return axios({
    url: `/api/component/pageByArtifact`,
    method: "get",
    params: data,
  });
}

// 项目组件
export function getProjectsService(id, data) {
  return axios({
    url: `${preApi}/v1/service/project/${id}`,
    method: "get",
    params: data,
  });
}

// 漏洞审查
export function getFindingProject(id, data) {
  return axios({
    url: `${preApi}/v1/finding/project/${id}`,
    method: "get",
    params: data,
  });
}

// 漏洞列表
export function getVulnerability(data) {
  return axios({
    url: `/api/vulnerability/pageByArtifact`,
    method: "get",
    params: data,
  });
}

// 漏洞利用预测
export function getFindingProjectNVD(id, data) {
  return axios({
    url: `${preApi}/v1/finding/project/${id}`,
    method: "get",
    params: data,
  });
}

// 违反政策
export function getViolationProjects(id, data) {
  return axios({
    url: `${preApi}/v1/violation/project/${id}`,
    method: "get",
    params: data,
  });
}
export function addProject(data) {
  return axios({
    url: `${preApi}/v1/project`,
    method: "put",
    data: data,
  })
}
export function downProjects(id, data) {
  return axios({
    url: `${preApi}/v1/bom/cyclonedx/project/${id}`,
    method: "get",
    params: data,
    headers: {
      "Content-Type": "application/x-www-form-urlencoded"
    }
  })
}
export function getLicenseConcise() {
  return axios({
    url: `${preApi}/v1/license/concise`,
    method: "get"
  });
}
export function addProjectComponent(id, data) {
  return axios({
    url: `${preApi}/v1/component/project/${id}`,
    method: "put",
    data: data,
  })
}

export function uploadBom(data) {
  return axios({
    url: `${preApi}/v1/bom`,
    method: 'post',
    timeout: 15 * 60 * 1000,
    headers: {
      'Content-Type': "multipart/form-data",
    },
    data
  })
}

export function downloadBom(uuid,data) {
  return axios({
    url: `${preApi}/v1/bom/cyclonedx/project/${uuid}`,
    method: 'get',
    params: data
  })
}

export function getComponentsAndServices(uuid) {
  return axios({
    url: `${preApi}/v1/dependencyGraph/project/${uuid}/directDependencies`,
    method: 'get'
  })
}

export function getComponentsAndServicesByComponent(uuid) {
  return axios({
    url: `${preApi}/v1/dependencyGraph/component/${uuid}/directDependencies`,
    method: 'get'
  })
}

export function getAllByComponentId(uuid, componentId) {
  return axios({
    url: `${preApi}/v1/component/project/${uuid}/dependencyGraph/${componentId}`,
    method: 'get'
  })
}

/**
 * 添加组件评论
 * @param data
 * @returns {obj}
 */
export function addViolationAnalysis( data) {
  return axios({
    url: `${preApi}/v1/violation/analysis`,
    method: 'put',
    data: data
  })
}

/**
 * 删除项目
 * @param id
 * @param uuid
 * @returns {obj}
 */
export function deleteProject(uuid) {
  return axios({
    url: `${preApi}/v1/project/${uuid}`,
    method: 'delete'

  })
}

/**
 * 更新项目
 * @param data 更新数据
 * @returns {obj}
 */
export function updateProject(data) {
  return axios({
    url: `${preApi}/v1/project`,
    method: 'post',
    data: data
  })
}

/**
 * 克隆项目
 * @param data
 * @returns {obj}
 */
export function cloneProject(data) {
  return axios({
    url: `${preApi}/v1/project/clone`,
    method: 'put',
    data: data
  })
}

/**
 * 获取项目属性列表
 * @param projectUuid 项目uuid
 * @param searchText
 * @returns {obj}
 */
export function getProjectPropertyList(projectUuid,searchText) {
  return axios({
    url: `${preApi}/v1/project/${projectUuid}/property`,
    method: 'get',
    params:{
      searchText:searchText
    }
  })
}

/**
 * 添加项目属性
 * @param projectUuid 项目uuid
 * @param data
 * @returns {obj}
 */
export function addProjectProperty(projectUuid, data) {
  return axios({
    url: `${preApi}/v1/project/${projectUuid}/property`,
    method: 'put',
    data: data
  })
}

/**
 * 删除项目属性
 * @param projectUuid 项目uuid
 * @param data
 * @returns {obj}
 */
export function deleteProjectProperty(projectUuid, data) {
  return axios({
    url: `${preApi}/v1/project/${projectUuid}/property`,
    method: 'delete',
    data: data
  })
}

/**
 * 添加漏洞评论
 * @param data
 * @returns {obj}
 */
export function addVulnerabilitiesAnalysis(data) {
  return axios({
    url: `${preApi}/v1/analysis`,
    method: 'put',
    data: data
  })
}

/**
 * 获取漏洞评论
 * @param projectUuid
 * @param componentUuid
 * @param vulnerabilityUuid
 * @returns {obj}
 */
export function getVulnerabilitiesAnalysis(projectUuid, componentUuid, vulnerabilityUuid) {
  return axios({
    url: `${preApi}/v1/analysis`,
    method: 'get',
    params: {
      project: projectUuid,
      component: componentUuid,
      vulnerability: vulnerabilityUuid
    }
  })
}

/**
 * 返回特定组件的所有漏洞的列表
 * @param componentUUid
 * @param data
 * @returns {obj}
 */
export function  getVulnerabilitiesByComponent(componentUUid,data){
  return axios({
    url: `${preApi}/v1/vulnerability/component/${componentUUid}`,
    method: 'get',
    params: data
  })
}

/**
 * 更新组件
 * @param data
 * @returns {obj}
 */
export function  updateComponent(data){
  return axios({
    url: `${preApi}/v1/component`,
    method: 'post',
    data: data
  })
}

/**
 * 删除组件
 * @param uuid
 * @returns {obj}
 */
export function  deleteComponent(uuid){
  return axios({
    url: `${preApi}/v1/component/${uuid}`,
    method: 'delete'
  })
}

/**
 * 获取项目子项目列表
 * @param parentId
 * @param data
 * @returns {ary}
 */
export function getProjectChildrenList(parentId,data) {
  return axios({
    url: `${preApi}/v1/project/${parentId}/parent`,
    method: "get",
    params: data,
  });
}

///