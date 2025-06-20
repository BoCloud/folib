<template>
    <div class="wrapper">
        <a-card :bordered="false" class="header-solid h-full" :bodyStyle="{ padding: 0 }">
            <ChartEpssVsCvss :chartData="projectsData" class="chart-cvss"></ChartEpssVsCvss>
            <div class="table">
                <div class="mx-25 search">
                    <a-col :span="24" class="text-right">
                        <!-- <a-input-search
                            :placeholder="$t('Projects.EnterVulnerabilityNumber')"
                            class="v-search"
                            v-model="queryParams.searchText"
                            @search="handheTableSearch()"
                        /> -->
                    </a-col>
                </div>
                <a-table
                    rowKey="matrix"
                    class="mt-20"
                    :columns="i18nColumns"
                    :data-source="projectsData"
                    @change="handleChangeTable"
                    :pagination="{
            pageSize: queryParams.pageSize,
            current: queryParams.pageNumber,
            total: queryParams.total || 0,
            showLessItems: true,
          }"
                >
                    <template slot="name" slot-scope="name, row">
                        <a-button type="link" @click="handleGoCom(row)">
                            {{ name }}
                        </a-button>
                    </template>
                    <template slot="vulnId" slot-scope="vulnId, row">
                        <a-button type="link" @click="handleGoVul(row)">
                            {{ vulnId }}
                        </a-button>
                    </template>

                    <template slot="published" slot-scope="published">
                        {{ typeof published !== "undefined" ? formatTimestamp(published, true) : "-" }}
                    </template>
                    <template slot="severity" slot-scope="severity">
                        <div class="table-avatar-info" v-if="severity">
                            <a-avatar
                                v-if="['CRITICAL', 'MEDIUM', 'HIGH', 'LOW'].indexOf(severity) != -1"
                                :size="24"
                                :src="'images/folib/' + severity.toLowerCase() + '.svg'"
                            />
                            <a-avatar v-else shape="circle" :size="24">{{ severity.slice(0, 1) }}</a-avatar>
                            <div class="avatar-info">
                                <p class="mb-0 text-dark">
                                    {{
                                        severity === "CRITICAL"
                                            ? "严重"
                                            : severity === "MEDIUM"
                                                ? "中危"
                                                : severity === "HIGH"
                                                    ? "高危"
                                                    : severity === "LOW"
                                                        ? "低危"
                                                        : severity
                                    }}
                                </p>
                            </div>
                        </div>
                    </template>
                </a-table>
            </div>
        </a-card>
    </div>
</template>

<script>
import ChartEpssVsCvss from "./Components/ChartEpssVsCvss.vue"
import {getFindingProjectNVD} from "@/api/projects.js"

export default {
    components: {ChartEpssVsCvss},
    created() {
        this.getData()
    },
    computed: {
        i18nColumns() {
            return this.columns.map((column) => {
                if (column.i18nKey) {
                    column.title = this.$t(column.i18nKey)
                }
                return column
            })
        },
    },
    data() {
        return {
            columns: [
                {
                    i18nKey: "Projects.ComponentName",
                    title: "组件",
                    dataIndex: "component.name",
                    sorter: (a, b) => a.component.name.length - b.component.name.length,
                    scopedSlots: {customRender: "name"},
                    sortDirections: ["descend", "ascend"],
                },
                {
                    i18nKey: "Projects.Version",
                    title: "版本",
                    dataIndex: "component.version",
                    //   sorter: (a, b) => a.component.version - b.component.version,
                    sortDirections: ["descend", "ascend"],
                },
                {
                    i18nKey: "Projects.Group",
                    title: "组",
                    dataIndex: "component.group",
                    sorter: (a, b) => a.component.group.length - b.component.group.length,
                    sortDirections: ["descend", "ascend"],
                },
                {
                    i18nKey: "Projects.Vulnerability",
                    title: "漏洞编号",
                    dataIndex: "vulnerability.vulnId",
                    // sorter: true,
                    scopedSlots: {customRender: "vulnId"},
                    sortDirections: ["descend", "ascend"],
                },

                {
                    i18nKey: "Projects.CVSS",
                    title: "CVSS",
                    dataIndex: "vulnerability.cvssV3BaseScore",
                },
                {
                    i18nKey: "Projects.EPSS",
                    title: "每股收益",
                    dataIndex: "vulnerability.epssScore",
                },
                {
                    i18nKey: "Projects.EPSSPercentile",
                    title: "每股收益百分位数",
                    dataIndex: "vulnerability.epssPercentile",
                },
                {
                    i18nKey: "Projects.Suppressed",
                    title: "抑制",
                    dataIndex: "analysis.isSuppressed",
                },
            ],
            projectsData: [],
            queryParams: {
                pageNumber: 1,
                pageSize: 10,
                sortOrder: "",
                sortName: "",
                searchText: null,
                total: 0,
            },
        }
    },
    methods: {
        getData() {
            const uuid = this.$route.params.id
            getFindingProjectNVD(uuid, this.queryParams).then((res) => {
                // console.log(res)
                this.projectsData = res.data
            })
        },
        handleChangeTable(pagination, filters, sorter) {
            if (pagination) {
                this.queryParams.pageNumber = pagination.current
            }
            this.queryParams.sortName = sorter.field
            if (sorter && sorter.order === "descend") {
                this.queryParams.sortOrder = "desc"
            } else if (sorter && sorter.order === "ascend") {
                this.queryParams.sortOrder = "asc"
            } else {
                this.queryParams.sortOrder = ""
            }
            this.getData()
        },
        handheTableSearch() {
            this.queryParams.pageNumber = 1
            this.getData()
        },
        handleGoCom(row) {
            // console.log(row)
            this.$router.push(`/componentDetail/${row.component.uuid}`)
        },

        handleGoVul(row) {
            this.$router.push(`/vulnerabilities/vulnerabilitiesDetail/${row.vulnerability.vulnId}?source=2`)
        },
    },
}
</script>

<style lang="scss" scoped>
.wrapper {
    padding: 16px;
    margin-bottom: 20px;
}

.search {
    height: 50px;
}

.v-search {
    max-width: 200px;
    width: 170px;
    min-width: 150px;
    margin-left: 5px;
    margin-bottom: 8px;
}
</style>
