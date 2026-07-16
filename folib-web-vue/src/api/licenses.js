import { axios } from "@/utils/request";

// 获取license列表
export function getLicensesList(data) {
  return axios({
    url: `/api/license/page`,
    method: "get",
    params: data,
  });
}

// 获取license列表
export function getLicenses(data) {
  return axios({
    url: `/api/license/list`,
    method: "get",
    params: data,
  });
}

// 获取license详情
export function getLicenseDetail(data) {
  return axios({
    url: `/api/license/detail/${data}`,
    method: "get",
  });
}

// 设置license黑白名单
export function blackWhite(data) {
  return axios({
    url: `/api/license/blackWhite`,
    method: "post",
    data: data
  });
}
