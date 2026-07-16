import { axios } from "@/utils/request";

const preApi = "/dependency";

export function getDaysData() {
  return axios({
    url: `${preApi}/v1/metrics/portfolio/90/days`,
    method: "get",
  });
}
