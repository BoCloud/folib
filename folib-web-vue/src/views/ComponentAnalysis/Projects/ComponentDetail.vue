<template>
    <div class="component-detail">
        <a-card :bordered="false" class="by-m-b-20" >
            <a-row class="by-flex by-row-between">
                <a-col class="by-flex by-flex-1">
                    <a-icon
                        class="apartment-icon"
                        type="apartment"
                    />
                    <div class="h5 title">
                        <div>
                            {{ component.name || '-' }}
                        </div>
                        <div class="by-flex">
                            <div class="view-detail by-pointer by-m-r-10" @click="backGraph">
                                <a-icon type="sliders" />
                            </div>
                            <div class="view-detail by-pointer" @click="openDetailModal">{{ $t('Projects.ViewDetail') }}</div>
                        </div>
                    </div>
                </a-col>
                <a-col class="by-flex">
                    <vue-easy-pie-chart
                        style="margin-right: 1rem"
                        :bar-color="severityCritical"
                        font-size="14px"
                        :track-color="trackColor"
                        scale-color=""
                        line-cap="round"
                        :line-width="3"
                        :percent="100"
                        :size="50"
                        :animate="true"
                        :title="'危急'"
                    >
                        {{ currentCritical }}
                    </vue-easy-pie-chart>
                    <vue-easy-pie-chart
                        style="margin-right: 1rem"
                        :bar-color="severityHigh"
                        font-size="14px"
                        :track-color="trackColor"
                        scale-color=""
                        line-cap="round"
                        :line-width="3"
                        :percent="100"
                        :size="50"
                        :animate="true"
                        :title="'高'"
                    >
                        {{ currentHigh }}
                    </vue-easy-pie-chart>
                    <vue-easy-pie-chart
                        style="margin-right: 1rem"
                        :bar-color="severityMedium"
                        font-size="14px"
                        :track-color="trackColor"
                        scale-color=""
                        line-cap="round"
                        :line-width="3"
                        :percent="100"
                        :size="50"
                        :animate="true"
                        :title="'中等'"
                    >
                        {{ currentMedium }}
                    </vue-easy-pie-chart>
                    <vue-easy-pie-chart
                        style="margin-right: 1rem"
                        :bar-color="severityLow"
                        font-size="14px"
                        :track-color="trackColor"
                        scale-color=""
                        line-cap="round"
                        :line-width="3"
                        :percent="100"
                        :size="50"
                        :animate="true"
                        :title="'低'"
                    >
                        {{ currentLow }}
                    </vue-easy-pie-chart>
                    <vue-easy-pie-chart
                        style="margin-right: 1rem"
                        :bar-color="severityUnassigned"
                        font-size="14px"
                        :track-color="trackColor"
                        scale-color=""
                        line-cap="round"
                        :line-width="3"
                        :percent="100"
                        :size="50"
                        :animate="true"
                        :title="'未分配'"
                    >
                        {{ currentUnassigned }}
                    </vue-easy-pie-chart>
                </a-col>
            </a-row>
        </a-card>
        <a-tabs class="tabs-sliding" default-active-key="1" @change="handleChangeTabs">
            <a-tab-pane key="1" :tab="$t('Projects.Overview')">
                <ComponentOverview></ComponentOverview>
            </a-tab-pane>
            <a-tab-pane key="2" :tab="$t('Projects.Vulnerabilities')">
                <ComponentVulnerability></ComponentVulnerability>
            </a-tab-pane>
        </a-tabs>
        <ComponentDetailModal @handel="getComponentDetail" @regression="handleRegression" ref="detail"></ComponentDetailModal>
    </div>
</template>
<script>
import {getComponentHeaderDetail, getComponentCurrent, getVulnerabilitiesByComponent} from "@/api/projects";
import { valueWithDefault } from "@/utils/util";
import VueEasyPieChart from "vue-easy-pie-chart"
import "vue-easy-pie-chart/dist/vue-easy-pie-chart.css"
import ComponentOverview from "./Components/ComponentOverview.vue";
import ComponentVulnerability from "./Components/ComponentVulnerability.vue";
import ComponentDetailModal from "./Components/ComponentDetailModal.vue";

export default {
    name: "ComponentDetail",
    components: {
        ComponentOverview,
        ComponentVulnerability,
        ComponentDetailModal,
        VueEasyPieChart
    },
    data() {
        return {
            component: {},
            severityCritical: "#f86c6b",
            severityHigh: "#fd8c00",
            severityMedium: "#ffc107",
            severityLow: "#4dbd74",
            severityUnassigned: "#777777",
            severityInfo: "#20a8d8",
            trackColor: "#17232f",
            currentCritical: 0,
            currentHigh: 0,
            currentMedium: 0,
            currentLow: 0,
            currentUnassigned: 0,
            currentRiskScore: 0,
            uuid: ''
        }
    },
    created() {
        this.getComponentDetail()
    },
    watch: {
        $route() {
            this.getComponentDetail()
        },
    },
    methods: {
        openDetailModal() {
            this.$refs.detail.openModal(this.component)
        },
        backGraph() {
            if (!this.component.project?.uuid) return
            this.$router.push({
                path: `/projectsDetail/${this.component.project.uuid}`,
                query: {
                    componentId: this.uuid
                }
            })
        },
        getComponentDetail() {
            this.uuid = this.$route.params.id
            getComponentHeaderDetail(this.uuid).then(res => {
                this.component = res.data
            })

            getComponentCurrent(this.uuid).then(res => {
                this.currentCritical = valueWithDefault(res.data.critical, 0)
                this.currentHigh = valueWithDefault(res.data.high, 0)
                this.currentMedium = valueWithDefault(res.data.medium, 0)
                this.currentLow = valueWithDefault(res.data.low, 0)
                this.currentUnassigned = valueWithDefault(res.data.unassigned, 0)
                this.currentRiskScore = valueWithDefault(res.data.inheritedRiskScore, 0)
            })
        },
        //getVulnerabilitiesByComponent
        handleChangeTabs() {

        },
        handleRegression() {
            this.$router.push(`/components`)
        }
    }
}
</script>

<style scoped lang="scss">
.apartment-icon {
    width: 56px;
    height: 56px;
    font-size: 30px;
    background-color: #20a8d8;
    color: #fff;
    line-height: 60px;
    float: left;
    margin-right: 10px;
}
.title {
    margin-top: 2px;
}
.view-detail {
    font-size: 14px;
    font-weight: 500;
    margin-top: 3px;
}
</style>