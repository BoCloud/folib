<template>
    <div>
        <a-row :gutter="16">
            <a-col class="gutter-row" :span="6">
                <a-card :bordered="false" class="widget-1">
                    <a-statistic
                        :title="$t('AdvancementCockpits.SuccessCount')"
                        :value="artifactSyncRecordCountData.successCount ? artifactSyncRecordCountData.successCount : 0"
                        :precision="0"
                        class="text-success"
                    >
                    </a-statistic>
                    <div class="icon" style="background-color: #52c41a">
                        <a-icon type="check-circle" theme="filled" two-tone-color="#52c41a"/>
                    </div>
                </a-card>
            </a-col>
            <a-col class="gutter-row" :span="6">
                <a-card :bordered="false" class="widget-1">
                    <a-statistic
                        :title="$t('AdvancementCockpits.FailedCount')"
                        :value="artifactSyncRecordCountData.failedCount ? artifactSyncRecordCountData.failedCount : 0"
                        :precision="0"
                        class="text-success"
                    >
                        <template #prefix></template>
                    </a-statistic>
                    <div class="icon" style="background-color: #f50">
                        <a-icon type="close-circle" theme="filled"/>
                    </div>
                </a-card>
            </a-col>
            <a-col class="gutter-row" :span="6">
                <a-card :bordered="false" class="widget-1">
                    <a-statistic
                        :title="$t('AdvancementCockpits.TotalCount')"
                        :value="artifactSyncRecordCountData.totalCount ? artifactSyncRecordCountData.totalCount : 0"
                        :precision="0"
                        class="text-success"
                    >
                        <template #prefix></template>
                    </a-statistic>
                    <div class="icon">
                        <a-icon type="experiment" theme="filled"/>
                    </div>
                </a-card>
            </a-col>
            <a-col class="gutter-row" :span="6">
                <a-card :bordered="false" class="widget-1">
                    <a-statistic
                        :title="$t('AdvancementCockpits.FileSizeCount')"
                        :value="artifactSyncRecordCountData ? artifactSyncRecordCountData.fileSizeCount : 0"
                        class="text-success"
                    >
                        <template #prefix></template>
                    </a-statistic>
                    <div class="icon" style="background-color: orange">
                        <a-icon type="file" theme="filled"/>
                    </div>
                </a-card>
            </a-col>
        </a-row>
        <a-row :gutter="24" type="flex" align="stretch" style="margin-top: 30px">
            <a-col :span="24" :lg="10" class="mb-24">

                <!-- Active Users Card -->
                <a-card :bordered="false" class="dashboard-bar-chart header-solid">
                    <template #title>
                        <h6>{{ $t('AdvancementCockpits.30daysData') }}</h6>
                        <textOver
                            :text="$t('AdvancementCockpits.30DaysPromotionRanking')"
                            :max="55"
                        />
                        <!--                <p>{{  }}</p>-->
                    </template>
                    <chart-bar ref="volFolib" className="barChartRadialGradient" :height="355" :data="barChartData"></chart-bar>
                </a-card>
                <!-- Active Users Card -->

            </a-col>
            <a-col :span="24" :lg="14" class="mb-24">
                <a-card :bordered="false" class="dashboard-bar-line header-solid">
                    <template #title>
                        <h6>{{ $t('AdvancementCockpits.30daysData') }}</h6>
                        <textOver
                            :text="$t('AdvancementCockpits.TrendsInPromotionStatusWithin30')"
                            :max="55"
                        />
                        <!--                <p>{{  }}</p>-->
                    </template>
                    <template #extra>
                        <a-badge color="#f50" class="badge-dot-primary" :text="$t('AdvancementCockpits.FailedCount')"/>
                        <a-badge color="#7feb91" class="badge-dot-secondary"
                                 :text="$t('AdvancementCockpits.SuccessCount')"/>
                        <a-badge color="#0bb0d5" class="badge-dot-secondary"
                                 :text="$t('AdvancementCockpits.TotalCount')"/>
                    </template>
                    <a-card :bordered="false" class="dashboard-bar-line header-solid">
                        <chart-line ref="d30map" :height="310" :data="lineChartData"></chart-line>
                    </a-card>
                </a-card>
            </a-col>
        </a-row>
        <a-row>
            <div class="wrapper vulnerability-database">
                <a-card :bordered="false" style="margin-top: 20px; margin-bottom: 20px">
                    <div class="mx-25 search">
                        <a-col :span="22" class="text-right">
                            <a-input-search :placeholder="$t('AdvancementCockpits.EnterVulnerabilitySourceStorageId')"
                                            class="v-search" v-model="queryParams.storageId"
                                            @search="handheTableSearch()"/>
                        </a-col>
                        <a-col :span="2" class="text-right">
                            <a-input-search
                                :placeholder="$t('AdvancementCockpits.EnterVulnerabilitySourceRepositoryId')"
                                class="v-search" v-model="queryParams.repositoryId"
                                @search="handheTableSearch()"/>
                        </a-col>
                    </div>
                    <a-table rowKey="uuid" class="mt-20" size="middle" :columns="i18nColumns2"
                             :data-source="vulnerabilityDatabaseData"
                             @change="handleChangeTable" :scroll="{ x: true }" :loading="vulnerabilityTableLoading"
                             :pagination="{ pageSize: queryParams.pageSize, current: queryParams.pageNumber, total: queryParams.total, showLessItems: true }">
                        <template slot="status" slot-scope="status">
                            <a-tag
                                :color="status === 1 ? 'gray' : status === 2 ? 'blue' : status === 3 ? 'green' : 'red'">
                                {{ status === 1 ? $t('AdvancementCockpits.Ready') : status === 2 ? $t('AdvancementCockpits.Syncing') : status === 3 ? $t('AdvancementCockpits.Success') : $t('AdvancementCockpits.Failed') }}
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
                                {{ slaveRecordCleared ? $t('AdvancementCockpits.Removals') : $t('AdvancementCockpits.NotCleared') }}
                            </a-tag>
                        </template>
                        <template slot="opsType" slot-scope="opsType,row">
                            <a-tag :color="opsType ===1 ? 'orange' : 'purple'">
                                {{ opsType === 1 ? $t('AdvancementCockpits.ProductUpgrade') : opsType === 2 ? $t('AdvancementCockpits.Distribution') : $t('AdvancementCockpits.Uncharted') }}
                            </a-tag>
                        </template>
                        <template slot="syncModel" slot-scope="syncModel">
                            {{ (syncModel && syncModel === 1) ? 'push' : 'pull' }}
                        </template>
                        <div slot="failedReason"
                             slot-scope="text, record">
                            <template v-if="record.failedReason && record.status ===4">
                                <a-tooltip>
                                    <template slot="title">
                                        {{ record.failedReason }}
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
                                        {{ record.targetPath }}
                                    </template>
                                    <template v-if="record.opsType && record.opsType === 2">
                                        <template v-for="(info, index) in JSON.parse(record.targetPath)">
                                            {{ $t('Repository.DistributionNode') }}{{ index + 1 }}:
                                            {{ info.dispatchClusterEnName }}
                                            <template v-if="info.targetStorageId">
                                                &nbsp;&nbsp;{{ $t('Repository.StorageSpace') }}:
                                                {{ info.targetStorageId || '-' }}
                                            </template>
                                            <template v-if="info.targetRepositoryId">
                                                &nbsp;&nbsp;{{ $t('Repository.WarehouseName') }}:
                                                {{ info.targetRepositoryId || '-' }}
                                            </template>
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
                                              :cancel-text="$t('Repository.Cancel')"
                                              @confirm="confirmRecord(record)"
                                              @cancel="cancelRecord"
                                >
                                    <a-button type="link" v-if="record.status === 4"
                                              @click="confirmRecord(record)"
                                              size="small">
                                        <span class="text-danger">{{ $t('Repository.Compensation') }}</span>
                                    </a-button>

                                </a-popconfirm>
                                <a-popconfirm :title="getTitle()"
                                              okType="danger"
                                              :ok-text="$t('Repository.Confirm')"
                                              :cancel-text="$t('Repository.Cancel')"
                                              @confirm="updatePriority(record)"
                                              @cancel="cancelRecord"
                                >
                                    <a-button type="link" v-if="record.status === 1"
                                              @click="confirmRecord(record)"
                                              size="small">
                                        <span class="text-danger">{{ $t('AdvancementCockpits.SetTop') }}</span>
                                    </a-button>
                                </a-popconfirm>
                            </div>
                        </div>
                    </a-table>
                </a-card>
            </div>
        </a-row>
    </div>


</template>

<script>
import { Chart } from 'chart.js';
import ChartBar from '@/components/Charts/ChartBar';
import ChartLine from '@/components/Charts/ChartLine';
import {formatTimestamp} from "@/utils/util.js";
import {
    getArtifactSyncRecordPage,
    getArtifactSyncRecordCount,
    getStatusTrends,
    fileSizeStatisticsByWarehouse
} from "@/api/settings";
import {retryAtifactDispatch, retryNodeOption} from "@/api/artifact";
import textOver from "@/components/Tools/textOver";

export default {
    components: {
        ChartBar,
        ChartLine,
        textOver,
    },
    data() {
        return {
            columns2: [
                {
                    title: "制品同步编号",
                    width: 130,
                    i18nKey: 'AdvancementCockpits.SyncNo',
                    dataIndex: "syncNo",
                    scopedSlots: {customRender: "syncNo"},
                },
                {

                    title: "源存储空间",
                    width: 110,
                    i18nKey: 'AdvancementCockpits.SourceStorageId',
                    dataIndex: "sourceStorageId",
                    scopedSlots: {customRender: "sourceStorageId"},
                },
                {

                    title: "源仓库",
                    width: 110,
                    i18nKey: 'AdvancementCockpits.SourceRepositoryId',
                    dataIndex: "sourceRepositoryId",
                    scopedSlots: {customRender: "sourceRepositoryId"},
                },

                {
                    title: "源制品路径",
                    width: 110,
                    i18nKey: 'AdvancementCockpits.SourcePath',
                    dataIndex: "sourcePath",
                    scopedSlots: {customRender: "sourcePath"},
                },
                {
                    title: "同步模式",
                    width: 110,
                    i18nKey: 'AdvancementCockpits.SynchronousMode',
                    dataIndex: "syncModel",
                    scopedSlots: {customRender: "syncModel"},
                },
                {
                    title: "目标制品路径信息",
                    width: 130,
                    i18nKey: 'AdvancementCockpits.TargetPath',
                    dataIndex: "targetPath",
                    scopedSlots: {customRender: "targetPath"},
                },
                {
                    title: "制品操作",
                    width: 110,
                    i18nKey: 'AdvancementCockpits.OpsType',
                    dataIndex: "opsType",
                    scopedSlots: {customRender: "opsType"},
                },
                {
                    title: "同步状态",
                    width: 110,
                    i18nKey: 'AdvancementCockpits.Status',
                    dataIndex: "status",
                    scopedSlots: {customRender: "status"},
                },
                {
                    title: "从记录状态",
                    width: 130,
                    i18nKey: 'AdvancementCockpits.SlaveRecordCleared',
                    dataIndex: "slaveRecordCleared",
                    scopedSlots: {customRender: "slaveRecordCleared"},
                },
                {
                    title: "同步进度",
                    width: 110,
                    i18nKey: 'AdvancementCockpits.SyncProgress',
                    dataIndex: "syncProgress",
                    scopedSlots: {customRender: "syncProgress"},
                },
                {
                    title: "失败原因",
                    width: 110,
                    i18nKey: 'AdvancementCockpits.FailedReason',
                    dataIndex: "failedReason",
                    scopedSlots: {customRender: "failedReason"},
                },
                {
                    title: "创建时间",
                    width: 140,
                    i18nKey: 'AdvancementCockpits.CreateTime',
                    dataIndex: "createTime",
                    scopedSlots: {customRender: "createTime"},
                },
                {
                    title: "操作",
                    width: 60,
                    i18nKey: 'AdvancementCockpits.Operational',
                    dataIndex: "operation",
                    scopedSlots: {customRender: "operation"},
                },

            ],
            vulnerabilityDatabaseData: [],
            currentClickRecord: null,
            vulnerabilityTableLoading: false,
            artifactSyncRecordCountData: {},
            queryParams: {
                pageNumber: 1,
                pageSize: 10,
                storageId: "",
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
            barChartData: {
                labels: [], // 示例标签
                datasets: [{
                    label: '总文件大小(G)',
                    backgroundColor: '#fff',
                    borderWidth: 0,
                    borderSkipped: false,
                    borderRadius: 6,
                    data: [], // 示例数据
                    maxBarThickness: 20,
                }],
            },
            // chartOptions: {
            //     responsive: true,
            //     maintainAspectRatio: false,
            //     plugins: {
            //         legend: {
            //             backgroundColor: '#02f446' // 图例背景颜色
            //         }
            //     },
            //     // scales: {
            //     //     x: {
            //     //         backgroundColor: '#ffffff', // X轴背景颜色
            //     //         grid: {
            //     //             backgroundColor: '#f0f0f0' // X轴网格背景颜色
            //     //         }
            //     //     },
            //     //     y: {
            //     //         beginAtZero: true,
            //     //         backgroundColor: '#b41d1d', // Y轴背景颜色
            //     //         grid: {
            //     //             backgroundColor: '#b41d1d' // Y轴网格背景颜色
            //     //         }
            //     //     }
            //     // },
            //     backgroundColor: '#02f40f' // 图表整体背景颜色
            // },
            chartOptions: {
                plugins: {
                    customCanvasBackgroundColor: {
                        color: 'lightGreen',
                    }
                }
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
        lineChartData() {
            return {
                labels: [],
                datasets: [{
                    label: this.$t('AdvancementCockpits.SuccessCount'),
                    tension: 0.4,
                    pointRadius: 0,
                    borderColor: "#7feb91",
                    borderWidth: 1,
                    data: [],
                    maxBarThickness: 6

                },
                    {
                        label: this.$t('AdvancementCockpits.FailedCount'),
                        tension: 0.4,
                        pointRadius: 0,
                        borderColor: "#f50",
                        borderWidth: 1,
                        data: [],
                        maxBarThickness: 6

                    }
                    ,
                    {
                        label: this.$t('AdvancementCockpits.TotalCount'),
                        tension: 0.2,
                        pointRadius: 0,
                        borderColor: "#0bb0d5",
                        borderWidth: 1,
                        data: [],
                        maxBarThickness: 6

                    }
                ],
            }
        }
    },

    watch: {
        eventPageVisible: function (val) {
            if (val) {
                this.getData()
                this.intervalId = setInterval(() => {
                    this.getData()
                }, 3000);
            } else {
                if (this.intervalId) {
                    clearInterval(this.intervalId);
                    this.intervalId = undefined
                }
            }
        }
    },
    created() {

    },
    mounted() {
        this.getData();
        this.getCountData();
        this.getStatusTrendsData();
        this.fileSizeStatisticsByWarehouseData();
        this.chart = new Chart(this.$refs.volFolib, {
            type: 'bar',
            data: this.chartData,
            options: this.chartOptions
        });
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
        getTitle() {
            return  this.$t('Repository.CurrentProductIsSynchronizing')+this.$t('AdvancementCockpits.SetTop');
        },
        fileSizeStatisticsByWarehouseData() {
            let days = 30;
            let limitNumber = 10;
            fileSizeStatisticsByWarehouse(days, limitNumber).then(res => {
                res.forEach((item) => {

                    this.barChartData.labels.push(item.repositoryId)
                    this.barChartData.datasets[0].data.push(item.fileSize)
                    console.log("barChartData", this.barChartData)
                })
                if (this.$refs.volFolib) {
                    this.$refs.volFolib.buildData()
                }
            })
        },
        getStatusTrendsData() {
            getStatusTrends().then(res => {
                if (res) {
                    res.forEach((item) => {
                        this.lineChartData.labels.push(item.date)
                        this.lineChartData.datasets[0].data.push(item.successCount)
                        this.lineChartData.datasets[1].data.push(item.failedCount)
                        this.lineChartData.datasets[2].data.push(item.totalCount)
                    })
                    if (this.$refs.d30map) {
                        this.$refs.d30map.buildData()
                    }
                }
            })
        },

        // 获取表格数据
        getData() {
            //this.vulnerabilityTableLoading = true
            getArtifactSyncRecordPage(this.queryParams)
                .then(res => {
                    this.dataFilter.total = res.data.total
                    this.queryParams.total = res.data.total
                    this.vulnerabilityDatabaseData = res.data.rows
                }).finally(() => {
                //this.vulnerabilityTableLoading = false
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
        clickRecord(v){
            this.currentClickRecord = v;
        },
        confirmRecord(v) {
             this.currentClickRecord = v
            let sycnNo = this.currentClickRecord.syncNo;
            //1：制品晋级；2：制品分发
            let opsType = this.currentClickRecord.opsType;
            if (opsType === 1) {
                this.vulnerabilityTableLoading = true
                retryNodeOption(sycnNo).then(res => {
                    this.$message.success("success");
                    this.getData();
                }).finally(() => {
                    this.vulnerabilityTableLoading = false
                });
            } else if (opsType === 2) {
                const jsonArrayString = JSON.parse(this.currentClickRecord.targetPath);
                let type = jsonArrayString[0].artifactoryRepositoryType;
                retryAtifactDispatch(sycnNo, type).then(res => {
                    this.$message.success("success");
                    this.getData();
                }).finally(() => {
                    this.vulnerabilityTableLoading = false
                });
            }

        },
        cancelRecord(){
            this.$message.warning("cancel")
        },
        updatePriority(v) {
            this.currentClickRecord = v
            let sycnNo = this.currentClickRecord.syncNo;
            updateTaskQueuePriority(sycnNo, 0).then(res => {
                this.vulnerabilityTableLoading = true
                this.$message.success("success");
                this.getData();
            }).finally(() => {
                this.vulnerabilityTableLoading = false
            })

        },
        getCountData() {
            getArtifactSyncRecordCount().then(res => {
                console.log("data", res)
                this.artifactSyncRecordCountData = res
            }).finally(() => {
                console.log("artifactSyncRecordCountData", this.artifactSyncRecordCountData)
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
