<template>
    <div class="wrapper vulnerability-database">
        <a-card :bordered="false" style="margin-top: 20px; margin-bottom: 20px">
            <div class="mx-25 search">
                <a-col :span="22" class="text-right">
                    <a-input-search :placeholder="$t('AdvancementCockpits.EnterVulnerabilitySourceStorageId')" class="v-search" v-model="queryParams.storageId"
                                    @search="handheTableSearch()" />
                </a-col>
                <a-col :span="2" class="text-right">
                    <a-input-search :placeholder="$t('AdvancementCockpits.EnterVulnerabilitySourceRepositoryId')" class="v-search" v-model="queryParams.repositoryId"
                                    @search="handheTableSearch()" />
                </a-col>
            </div>
            <a-table rowKey="uuid" class="mt-20" size="middle" :columns="i18nColumns2" :data-source="vulnerabilityDatabaseData"
                     @change="handleChangeTable" :scroll="{ x: true }" :loading="vulnerabilityTableLoading"
                     :pagination="{ pageSize: queryParams.pageSize, current: queryParams.pageNumber, total: queryParams.total, showLessItems: true }">
                <template slot="status" slot-scope="status">
                    <a-tag :color="status === 1 ? 'gray' : status === 2 ? 'blue' : status === 3 ? 'green' : 'red'">
                        {{ status === 1 ? $t('AdvancementCockpits.Ready') : status === 2 ? $t('AdvancementCockpits.Syncing') : status === 3 ? $t('AdvancementCockpits.Success') : $t('AdvancementCockpits.Failed')}}
                    </a-tag>
                </template>
                <template slot="syncProgress" slot-scope="syncProgress,row">
                    <a-progress type="circle"
                                :width="40"
                                :percent="(syncProgress * 100)"
                                :status="row.status === 4 ? 'exception': row.status === 3 ? 'success' : row.status === 2 ? 'normal': 'active' "
                                size="small"
                    />
                </template>
                <template slot="slaveRecordCleared" slot-scope="slaveRecordCleared,row">
                    <a-tag :color="row.slaveRecordCleared ? 'red' : 'gray'">
                        {{ slaveRecordCleared ? $t('AdvancementCockpits.Removals') : $t('AdvancementCockpits.NotCleared')}}
                    </a-tag>
                </template>
                <template slot="opsType" slot-scope="opsType,row">
                    <a-tag :color="opsType ===1 ? 'orange' : 'purple'">
                        {{ opsType ===1 ? $t('AdvancementCockpits.ProductUpgrade') : opsType ===2 ? $t('AdvancementCockpits.Distribution') : $t('AdvancementCockpits.Uncharted')}}
                    </a-tag>
                </template>
                <template slot="syncModel" slot-scope="syncModel">
                    {{(syncModel && syncModel ===1) ? 'push':'pull'}}
                </template>
                <div slot="failedReason"
                     slot-scope="text, record">
                    <template v-if="record.failedReason && record.status ===4">
                        <a-tooltip>
                            <template slot="title">
                                {{record.failedReason}}
                            </template>
                            <a>
                                <p class="copy-p">
                                    {{ $t('Repository.CheckOut') }}
                                </p>
                            </a>
                        </a-tooltip>
                    </template>
                    <template v-else>
                        -
                    </template>
                </div>
                <div slot="targetPath"
                     slot-scope="text, record">
                    <a-tooltip>
                        <template slot="title">
                            <template v-if="record.opsType &&  record.opsType === 1">
                                {{record.targetPath}}
                            </template>
                            <template v-if="record.opsType && record.opsType === 2">
                                <template v-for="(info, index) in JSON.parse(record.targetPath)">
                                    {{ $t('Repository.DistributionNode') }}{{index+1}}: {{info.dispatchClusterEnName}}
                                    <template v-if="info.targetStorageId">&nbsp;&nbsp;{{ $t('Repository.StorageSpace') }}: {{info.targetStorageId||'-'}}</template>
                                    <template v-if="info.targetRepositoryId">&nbsp;&nbsp;{{ $t('Repository.WarehouseName') }}: {{info.targetRepositoryId||'-'}}</template>
                                    <br/>
                                </template>
                            </template>
                        </template>
                        <a>
                            <p class="copy-p">
                                {{ $t('Repository.CheckOut') }}
                            </p>
                        </a>
                    </a-tooltip>
                </div>
                <div slot="operation"
                     slot-scope="text, record">
                    <div class="col-action">
                        <a-popconfirm :title="getProductStatusMessage()"
                                      okType="danger"
                                      :ok-text="$t('Repository.Confirm')"
                                      :cancel-text="$t('Repository.Cancel')">
                            <a-button type="link" @click="clickRecord(record)" v-if="record.status === 4"
                                      size="small">
                                <span class="text-danger">{{ $t('Repository.Compensation') }}</span>
                            </a-button>

                        </a-popconfirm>
                        <a-popconfirm :title="getTitle()"
                                      okType="danger"
                                      :ok-text="$t('Repository.Confirm')"
                                      :cancel-text="$t('Repository.Cancel')">
                            <a-button type="link" v-if="record.status === 1 || record.status === 2" @click="updatePriority(record)"
                                      size="small">
                                <span class="text-danger">{{ $t('AdvancementCockpits.SetTop') }}</span>
                            </a-button>
                        </a-popconfirm>
                    </div>
                </div>
            </a-table>
        </a-card>
    </div>
</template>

<script>
import { getVulnerabilitiesList } from "@/api/vulnerabilities.js";
import { formatTimestamp } from "@/utils/util.js";
import {getArtifactSyncRecordPage, getArtifactSyncRecordStatisticsPage} from "@/api/settings";
import {retryAtifactDispatch, retryNodeOption} from "@/api/artifact";


export default {
    components: {},
    data() {
        return {
            columns2: [
                {
                    title: "制品同步编号",
                    width: 130,
                    i18nKey: 'AdvancementCockpits.SyncNo',
                    dataIndex: "syncNo",
                    scopedSlots: { customRender: "syncNo" },
                },
                {

                    title: "源存储空间",
                    width: 110,
                    i18nKey: 'AdvancementCockpits.SourceStorageId',
                    dataIndex: "sourceStorageId",
                    scopedSlots: { customRender: "sourceStorageId" },
                },
                {

                    title: "源仓库",
                    width: 110,
                    i18nKey: 'AdvancementCockpits.SourceRepositoryId',
                    dataIndex: "sourceRepositoryId",
                    scopedSlots: { customRender: "sourceRepositoryId" },
                },

                {
                    title: "源制品路径",
                    width: 110,
                    i18nKey: 'AdvancementCockpits.SourcePath',
                    dataIndex: "sourcePath",
                    scopedSlots: { customRender: "sourcePath" },
                },
                {
                    title: "同步模式",
                    width: 110,
                    i18nKey: 'AdvancementCockpits.SynchronousMode',
                    dataIndex: "syncModel",
                    scopedSlots: { customRender: "syncModel" },
                },
                {
                    title: "目标制品路径信息",
                    width: 130,
                    i18nKey: 'AdvancementCockpits.TargetPath',
                    dataIndex: "targetPath",
                    scopedSlots: { customRender: "targetPath" },
                },
                {
                    title: "制品操作",
                    width: 110,
                    i18nKey: 'AdvancementCockpits.OpsType',
                    dataIndex: "opsType",
                    scopedSlots: { customRender: "opsType" },
                },
                {
                    title: "同步状态",
                    width: 110,
                    i18nKey: 'AdvancementCockpits.Status',
                    dataIndex: "status",
                    scopedSlots: { customRender: "status" },
                },
                {
                    title: "从记录状态",
                    width: 130,
                    i18nKey: 'AdvancementCockpits.SlaveRecordCleared',
                    dataIndex: "slaveRecordCleared",
                    scopedSlots: { customRender: "slaveRecordCleared" },
                },
                {
                    title: "同步进度",
                    width: 110,
                    i18nKey: 'AdvancementCockpits.SyncProgress',
                    dataIndex: "syncProgress",
                    scopedSlots: { customRender: "syncProgress" },
                },
                {
                    title: "失败原因",
                    width: 110,
                    i18nKey: 'AdvancementCockpits.FailedReason',
                    dataIndex: "failedReason",
                    scopedSlots: { customRender: "failedReason" },
                },
                {
                    title: "创建时间",
                    width: 140,
                    i18nKey: 'AdvancementCockpits.CreateTime',
                    dataIndex: "createTime",
                    scopedSlots: { customRender: "createTime" },
                },
                {
                    title: "操作",
                    width: 60,
                    i18nKey: 'AdvancementCockpits.Operational',
                    dataIndex: "operation",
                    scopedSlots: { customRender: "operation" },
                },

            ],
            vulnerabilityDatabaseData: [],
            currentClickRecord: null,
            vulnerabilityTableLoading: false,
            queryParams: {
                pageNumber: 1,
                pageSize: 10,
                storageId:"",
                repositoryId: "",
                sortOrder: "",
                sortName: "",
                total: 0,
            },
            dataFilter: {
                storageId: "",
                repositoryId: "",
                pageNumber: 1,
                pageSize: 20,
                total: 0
            },
        };
    },
    computed: {
        i18nColumns2() {
            return this.columns2.map(column => {
                if (column.i18nKey) {
                    column.title = this.$t(column.i18nKey);
                }
                return column;
            })
        },
    },
    created() {
        this.getData()
    },
    methods: {
        formatTimestamp,
        getProductStatusMessage() {
            if (this.currentClickRecord != null && this.currentClickRecord.status === 4) {
                return this.$t('Repository.CurrentProductIsSynchronizing') + this.$t('Repository.SureMakeProductCompensation');
            } else {
                return this.$t('Repository.SureMakeProductCompensation');
            }
        },
        getTitle(){
            if (this.currentClickRecord && (this.currentClickRecord.status === 1  || this.currentClickRecord.status ===2)) {
                this.$t('Repository.CurrentProductIsSynchronizing')
            }else{
                this.$t('AdvancementCockpits.SetTop');
            }
        },
        // 获取表格数据
        getData() {
            this.vulnerabilityTableLoading = true
            getArtifactSyncRecordPage(this.queryParams)
                .then(res => {
                    this.dataFilter.total = res.data.total
                    this.queryParams.total =  res.data.total
                    this.vulnerabilityDatabaseData = res.data.rows
                }).finally(() => {
                    this.vulnerabilityTableLoading = false
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
        handleGoDetail(row) {
            this.$router.push(`/vulnerabilities/vulnerabilitiesDetail/${row.cve}`)
        },
        handheTableSearch() {
            this.queryParams.pageNumber = 1
            this.getData()
        },
        clickRecord(v) {
            this.currentClickRecord = v
            let sycnNo = this.currentClickRecord.syncNo;
            //1：制品晋级；2：制品分发
            let opsType = this.currentClickRecord.opsType;
            if(opsType === 1){
                this.vulnerabilityTableLoading = true
                retryNodeOption(sycnNo).then(res =>{
                        this.$message.success("操作成功");
                        this.handleChangeTable();
                }).finally(() => {
                    this.vulnerabilityTableLoading = false
                });
            }else if(opsType === 2){
                const jsonArrayString = JSON.parse(this.currentClickRecord.targetPath);
                let type = jsonArrayString[0].artifactoryRepositoryType;
                retryAtifactDispatch(sycnNo,type).then(res =>{
                    this.$message.success("操作成功");
                    this.handleChangeTable();
                }).finally(() => {
                    this.vulnerabilityTableLoading = false
                });
            }

            console.log("currentClickRecord:",this.currentClickRecord)
        },
        updatePriority(v){
            this.currentClickRecord = v
            let sycnNo = this.currentClickRecord.syncNo;
            this.vulnerabilityTableLoading = true
            updateTaskQueuePriority(sycnNo,0).then(res =>{
                this.$message.success("操作成功");
                this.handleChangeTable();
            }).finally(() => {
                this.vulnerabilityTableLoading = false
            })


        }
    },
};
</script>

<style lang="scss" scoped>
.vulnerability-database::v-deep {
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

    .table-avatar-info .ant-avatar {
        margin-right: 8px;
    }
}
</style>
