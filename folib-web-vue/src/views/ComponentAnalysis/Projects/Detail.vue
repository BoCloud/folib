<template>
    <div>
        <a-card :bordered="false" style="margin-bottom: 20px">
            <a-row style="display: flex; justify-content: space-between">
                <a-col style="flex: 1; display: flex">
                    <a-icon
                        style="
              width: 56px;
              height: 56px;
              font-size: 30px;
              background-color: #20a8d8;
              color: #fff;
              line-height: 60px;
              float: left;
              margin-right: 10px;
            "
                        type="apartment"
                    />
                    <div class="h5 title">
                        <div>
                            {{ project.name || '-' }}
                            <ol
                                v-if="project.version"
                                style="display: inline-block; margin: 0; list-style-type: none; padding-inline-start: 0"
                            >
                                <li class="dropdown">
                                    <a-dropdown>
                                        <a class="ant-dropdown-link"> {{ showVersion }}
                                            <a-icon type="down"/>
                                        </a>
                                        <a-menu slot="overlay">
                                            <a-menu-item v-for="p in availableProjectVersions" :key="p.uuid"
                                                         @click="handleClickMenu(p)">
                                                <a href="javascript:;">{{ p.version }}</a>
                                            </a-menu-item>
                                        </a-menu>
                                    </a-dropdown>
                                </li>
                            </ol>
                        </div>
                        <div class="view-detail by-pointer"  @click="openDetailModal">{{ $t('Projects.ViewDetail') }}</div>
                    </div>
                    <!-- <div class="text-muted text-lowercase font-weight-bold font-xs">
                      <span v-for="tag in project.tags">
                        <a-badge :to="{ name: 'Projects', query: { tag: tag.name } }" variant="tag">{{ tag.name }}</a-badge>
                      </span>
                    </div> -->
                </a-col>
                <a-col style="display: flex">
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
                    >{{ currentCritical }}
                    </vue-easy-pie-chart
                    >
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
                    >{{ currentHigh }}
                    </vue-easy-pie-chart
                    >
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
                    >{{ currentMedium }}
                    </vue-easy-pie-chart
                    >
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
                    >{{ currentLow }}
                    </vue-easy-pie-chart
                    >
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
                    >{{ currentUnassigned }}
                    </vue-easy-pie-chart
                    >
                </a-col>
            </a-row>
        </a-card>
        <!-- <a-card :bordered="false" class="header-solid h-full" :bodyStyle="{ padding: 0 }"> -->
        <a-tabs class="tabs-sliding" v-model="tabActive" @change="handleChangeTabs">
            <a-tab-pane key="1" :tab="(source==1?$t('Projects.Repository'):$t('Projects.Artifact')) + $t('Projects.GeneralView')">
                <ProjectDashboard v-if="tabActive == 1"></ProjectDashboard>
            </a-tab-pane>
            <a-tab-pane key="2" :tab="$t('Projects.Component')" v-if="this.source!=1">
                <ProjectComponents v-if="tabActive == 2"></ProjectComponents>
            </a-tab-pane>
            <!-- <a-tab-pane key="3" :tab="$t('Projects.Services')" v-if="this.source!=1">
                <ProjectServices v-if="tabActive == 3"></ProjectServices>
            </a-tab-pane> -->
            <a-tab-pane key="7" :tab="$t('Projects.DependencyGraph')" v-if="this.source!=1">
                <DependencyGraph v-if="tabActive == 7" :uuid="uuid" :pro-name="project.name"></DependencyGraph>
            </a-tab-pane>
            <a-tab-pane key="4" :tab="$t('Projects.AuditVulnerabilities')" v-if="this.source!=1">
                <ProjectFindings v-if="tabActive == 4"></ProjectFindings>
            </a-tab-pane>
            <a-tab-pane key="5" :tab="$t('Projects.ExploitPredictions')" v-if="this.source!=1">
                <ProjectEpss v-if="tabActive == 5"></ProjectEpss>
            </a-tab-pane>
            <a-tab-pane key="6" :tab="$t('Projects.PolicyViolations')" v-if="this.source!=1">
                <ProjectPolicyViolations v-if="tabActive == 6"></ProjectPolicyViolations>
            </a-tab-pane>
            <a-tab-pane v-if="this.source==1" key="8" :tab="$t('Projects.Artifacts')">
                <ProjectChildren v-if="tabActive == 8" :uuid="project.uuid"></ProjectChildren>
            </a-tab-pane>
            <div style="display: flex;" slot="tabBarExtraContent">
                <div style="margin-right: 16px;">
                    <!-- <a-button @click="handleUpload">
                        <a-icon type="upload"/>
                        {{ $t('Projects.UploadBOM') }}
                    </a-button> -->
                </div>
                <!-- <div>
                    <a-dropdown>
                        <a-menu slot="overlay" @click="handleMenuClick">
                            <a-menu-item key="1" >{{ $t('Projects.Inventory') }}</a-menu-item>
                            <a-menu-item key="2">{{ $t('Projects.InventoryWithVulnerabilities') }}</a-menu-item>
                        </a-menu>
                        <a-button style="margin-left: 8px"> {{ $t('Projects.DownloadBOM') }}
                            <a-icon type="down"/>
                        </a-button>
                    </a-dropdown>
                </div> -->
            </div>
        </a-tabs>
        <!-- </a-card> -->
        <upload-bom ref="uploadBom"></upload-bom>
        <project-detail-modal @handle="initialize" ref="proDetail"></project-detail-modal>
    </div>
</template>

<script>
import { getProjectsHeaderDetail, getProjectsCurrent, downloadBom } from "@/api/projects.js"
import VueEasyPieChart from "vue-easy-pie-chart"
import { valueWithDefault, download } from "@/utils/util"
import "vue-easy-pie-chart/dist/vue-easy-pie-chart.css"
import ProjectDashboard from "./ProjectDashboard.vue"
import ProjectComponents from "./ProjectComponents.vue"
import ProjectServices from "./ProjectServices.vue"
import ProjectFindings from "./ProjectFindings.vue"
import ProjectEpss from "./ProjectEpss.vue"
import ProjectPolicyViolations from "./ProjectPolicyViolations.vue"
import UploadBom from "./Components/UploadBom.vue"
import ProjectDetailModal from "./Components/ProjectDetailModal.vue";
import DependencyGraph from "./DependencyGraph.vue";
import ProjectChildren from "./ProjectChildren.vue";

export default {
    components: {
        VueEasyPieChart,
        ProjectDashboard,
        ProjectComponents,
        ProjectServices,
        ProjectFindings,
        ProjectEpss,
        ProjectPolicyViolations,
        ProjectChildren,
        UploadBom,
        DependencyGraph,
        ProjectDetailModal
    },

    data() {
        return {
            tabActive: '1',
            project: {},
            availableProjectVersions: [],
            showVersion: "",
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
            uuid: '',
            componentId: '',
            source: 1,
        }
    },
    created() {
        this.initialize()
    },
    mounted() {
        const { componentId } = this.$route.query
        this.source = this.$route.query.source
        this.componentId = componentId
        if (componentId) this.tabActive = '7'
    },
    watch: {
        $route(to, from) {
            //console.log("路由变化了")
            this.initialize()
        },
    },
    methods: {
        handleMenuClick(e) {
            const params = {
                format: 'json',
                variant: e.key === '1' ? 'inventory' : 'withVulnerabilities',
                download: true
            }
            downloadBom(this.uuid, params).then(res => {
                if (!res) return
                const blob = new Blob([JSON.stringify(res, null, 2)],{type:'application/json,charset=utf-8;'});
                download(blob, 'bom.json')
            })
        },
        initialize() {
            this.uuid = this.$route.params.id
            getProjectsHeaderDetail(this.uuid).then((res) => {
                this.project = res.data
                // console.log("project",this.project)
                this.showVersion = res.data.version
                this.availableProjectVersions = res.data.versions
            })
            getProjectsCurrent(this.uuid).then((res) => {
                this.currentCritical = valueWithDefault(res.data.critical, 0)
                this.currentHigh = valueWithDefault(res.data.high, 0)
                this.currentMedium = valueWithDefault(res.data.medium, 0)
                this.currentLow = valueWithDefault(res.data.low, 0)
                this.currentUnassigned = valueWithDefault(res.data.unassigned, 0)
                this.currentRiskScore = valueWithDefault(res.data.inheritedRiskScore, 0)
            })
        },

        handleClickMenu(p) {
            this.$router.push(`/projectsDetail/${p.uuid}`)
            this.initialize()
        },
        handleChangeTabs(val) {
            if (this.componentId)
                this.$router.replace(`/projectsDetail/${this.uuid}`)
            this.tabActive = val
        },
        handleUpload() {
            this.$refs.uploadBom.handleOpen(this.uuid)
        },
        openDetailModal() {
            this.$refs.proDetail.openModal(this.project)
        }
    },
}
</script>

<style lang="scss" scoped>
.title {
    margin-top: 2px;
}

.dropdown {
    margin-left: 10px;
}

// ::v-deep .vue-easy-pie-chart .inner-text {
//   position: absolute;
//   top: 0;
//   left: 0;
//   right: 0;
//   bottom: 0;
//   text-align: center;
//   display: block;
// }

::v-deep .ant-card-head {
    padding: 0 !important;
}

::v-deep .ant-tabs-nav-wrap {
    padding: 0 24px;
}

.view-detail {
    font-size: 14px;
    font-weight: 500;
    margin-top: 3px;
}
</style>
