<template>
    <div class="wrapper project-children">
        <a-card :bordered="false" style="margin-top: 20px; margin-bottom: 20px">
            <div class="mx-25 search">
                <a-col :span="24" class="text-right">
                    <a-cascader :placeholder="$t('Projects.ParentProjectQuery')" class="parent-project-query"  v-if="source==2"
                        v-model="queryParams.parentId"
                        :showSearch="{ parentProjectFilter }"
                        :allowClear="false"
                        :options="repositoryList" @change="parentProjectChange"/>
                    <a-input-search
                    :placeholder="$t('Projects.ArtifactNameQuery')"
                    class="v-search"
                    v-model="queryParams.searchText"
                    @search="handheTableSearch()"
                    />
                </a-col>
            </div>
            <a-table
                rowKey="uuid"
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
                    <a-tooltip placement="rightTop">
                        <template slot="title">
                            {{ name }}
                        </template>
                        <a-button type="link" @click="handleGoDetail(row)">
                            {{    omitName(name) }}
                        </a-button>
                    </a-tooltip>

                </template>
                <template slot="license" slot-scope="license, row">
                    <a-button type="link" @click="handleGoLicense(row)">
                        {{ license }}
                    </a-button>
                </template>
                <template slot="lastBomImport" slot-scope="lastBomImport">
                    {{
                        typeof lastBomImport === "number" ? formatTimestamp(lastBomImport, true, $t('Projects.FormatTime')) : "-"
                    }}
                </template>
                <template slot="policyViolations" slot-scope="policyViolations">
                    <a-tooltip placement="left" >
                        <template slot="title" >
                            <div style="width: 120px;">
                                <p class="policyPromptTitle">{{ $t('Projects.PolicyType') }}</p>
                                <p class="policyPromptText">{{ $t('Projects.LicenseViolations') }}：{{
                                        policyViolations && policyViolations.policyViolationsLicenseTotal ? policyViolations.policyViolationsLicenseTotal : 0
                                    }} </p>
                                <p class="policyPromptText">{{ $t('Projects.OperationalViolations') }}： {{
                                        policyViolations && policyViolations.policyViolationsOperationalTotal ? policyViolations.policyViolationsOperationalTotal : 0
                                    }}</p>
                                <p class="policyPromptText">{{ $t('Projects.SecurityViolations') }}：{{
                                        policyViolations &&  policyViolations.policyViolationsSecurityTotal ? policyViolations.policyViolationsSecurityTotal : 0
                                    }}</p>
                                <p class="policyPromptTitle">{{ $t('Projects.ViolationState') }}</p>
                                <p class="policyPromptText">{{ $t('Projects.InformationalViolations') }}: {{
                                        policyViolations &&  policyViolations.policyViolationsInfo ? policyViolations.policyViolationsInfo : 0
                                    }}</p>
                                <p class="policyPromptText">{{ $t('Projects.ViolationWarnings') }}：{{
                                        policyViolations && policyViolations.policyViolationsWarn ? policyViolations.policyViolationsWarn : 0
                                    }}</p>
                                <p class="policyPromptText">{{ $t('Projects.ViolationFailures') }}：{{
                                        policyViolations && policyViolations.policyViolationsFail ? policyViolations.policyViolationsFail : 0
                                    }}</p>
                                <p class="policyPromptTitle">{{ $t('Projects.ViolationTotal') }}：{{
                                        policyViolations &&  policyViolations.policyViolationsTotal ? policyViolations.policyViolationsTotal : 0
                                    }}</p>
                            </div>

                        </template>
                        <a-tag color="#0bb0d5">{{ policyViolations &&  policyViolations.policyViolationsTotal ? policyViolations.policyViolationsTotal : 0  }}</a-tag>
                    </a-tooltip>

                </template>
                <template slot="vulnerabilities" slot-scope="vulnerabilities, row">
                    <a-tag color="#f86c6b">{{ row.metrics.critical }} </a-tag>
                    <a-tag color="#fd8c00">{{ row.metrics.high }} </a-tag>
                    <a-tag color="#ffc107">{{ row.metrics.medium }} </a-tag>
                    <a-tag color="#4dbd74"> {{ row.metrics.low }}</a-tag>
                    <a-tag color="#777777"> {{ row.metrics.unassigned }}</a-tag>
                </template>
            </a-table>
        </a-card>
    </div>
</template>

<script>
import {getProjectChildrenList, getProjectsService} from "@/api/projects.js"
import {queryBomOnScanTree} from "@/api/folib.js"
import { formatTimestamp } from "@/utils/util.js"
export default {
    components: {},
    computed: {
        i18nColumns() {
            return this.columns.map((column) => {
                if (column.i18nKey) {
                    column.title = this.$t(column.i18nKey)
                }
                return column
            })
        },
        lang() {
            return this.$store.state.language.lang
        },
    },
    props:{
        uuid: String,
        source: {
            type: Number,
            default: 1,
        },
    },

    data() {
        return {
            columns: [
                {
                    title: "项目名称",
                    i18nKey: "Projects.ProjectName",
                    dataIndex: "name",
                    sorter: false,
                    scopedSlots: {customRender: "name"},
                    sortDirections: ["descend", "ascend"],
                    width: "150px",
                },
                {
                    title: "上次导入清单",
                    i18nKey: "Projects.LastBOMImport",
                    dataIndex: "lastBomImport",
                    sorter: false,
                    scopedSlots: {customRender: "lastBomImport"},
                    sortDirections: ["descend", "ascend"],
                    width: "200px",
                },
                {
                    title: "清单格式",
                    i18nKey: "Projects.BOMFormat",
                    dataIndex: "lastBomImportFormat",
                    sorter: false,
                    width: "140px",
                    scopedSlots: {customRender: "lastBomImportFormat"},
                    sortDirections: ["descend", "ascend"],
                },
                {
                    title: "风险评分",
                    i18nKey: "Projects.RiskScore",
                    dataIndex: "lastInheritedRiskScore",
                    sorter: false,
                    width: "120px",
                    sortDirections: ["descend", "ascend"],
                },
                {
                    title: "违反政策",
                    i18nKey: "Projects.PolicyViolations",
                    dataIndex: "metrics",
                    sorter: false,
                    width: "150px",
                    scopedSlots: {customRender: "policyViolations"},
                    sortDirections: ["descend", "ascend"],
                },
                {
                    title: "漏洞",
                    i18nKey: "Projects.Vulnerabilities",
                    dataIndex: "vulnerabilities",
                    width: "230px",
                    scopedSlots: {customRender: "vulnerabilities"},
                },
            ],
            projectsData: [],
            queryParams: {
                pageNumber: 1,
                pageSize: 10,
                total: 0,
                searchText: "",
                parentId: [],
            },
            repositoryList: [],
            parentId: this.uuid,
        }
    },
    created() {
        this.initData()
    },
    methods: {
        formatTimestamp,
        queryBomOnScanTreeList() {
            this.repositoryList = []
            queryBomOnScanTree().then((res) => {
                if (res) {
                    this.repositoryList = res
                    if (!this.parentId && this.repositoryList) {
                        this.parentId = this.repositoryList[0].children[0].value
                        this.queryParams.parentId = [this.repositoryList[0].value, this.parentId]
                        this.getProjectData()
                    }
                }
            })
        },
        parentProjectChange(value) {
            this.parentId = value[1]
            this.getProjectData()
        },
        parentProjectFilter(inputValue, path) {
            return path.some(option => option.label.toLowerCase().indexOf(inputValue.toLowerCase()) > -1);
        },
        // 获取表格数据
        initData() {
            this.queryBomOnScanTreeList()
            this.getProjectData()
        },
        getProjectData() {
            const uuid = this.parentId
            if (!uuid) {
                return false
            }
            getProjectChildrenList(uuid, this.queryParams).then((res) => {
                this.queryParams.total = +res.headers["x-total-count"]
                this.projectsData = res.data === "" ? [] : res.data
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
            this.getProjectData()
        },
        handleGoDetail(row) {
            if(row.uuid && row.uuid !== this.parentId){
                this.$router.push(`/projectsDetail/${row.uuid}?source=2&t=` + Date.now())
            }
        },
        handleGoLicense(row) {
            if(row.uuid && row.uuid !== this.parentId){
                this.$router.push(`/projectsDetail/${row.uuid}?t=` + Date.now())
            }
        },
        handheTableSearch() {
            this.queryParams.pageNumber = 1
            this.getProjectData()
        },
        omitName(name){
            if(name.length>80){
                return name.substring(0,80)+"..."
            }else{
                return name;
            }
        }
    },
}
</script>

<style lang="scss" scoped>

.project-children {
    // max-width: 100%;
}
.search {
    height: 50px;
}
.mx-25 .ant-row-flex {
    flex-wrap: wrap;
}
.v-search {
    max-width: 200px;
    width: 170px;
    min-width: 150px;
    margin-left: 5px;
    margin-bottom: 8px;
}
.parent-project-query {
    min-width: 220px;
}
.parent-project-query::v-deep .ant-cascader-picker-label {
    padding: 0 30px 0 12px;
}
.policyPromptTitle{
    font-size: 16px;
    height: 10px;
}
.policyPromptText{
    font-size: 10px;
    height: 4px;
}
</style>
