<template>
    <div class="vulnerability-report">
        <a-drawer placement="right" width="65%" title="报告详情" :visible="visible" @close="close">
            <a-collapse default-active-key="1" :bordered="false" accordion>
                <template #expandIcon="props">
                    <a-icon type="caret-right" :rotate="props.isActive ? 90 : 0" />
                </template>
                <a-collapse-panel v-for="(item, index) in report" :key="index"
                    style="background: #f7f7f7;border-radius: 4px;margin-bottom: 24px;border: 0;overflow: hidden">
                    <template slot="header">
                        <div class="collapse-panel-header-info">
                            <span class="file-name">{{ item.fileName }}</span>
                            <a-tooltip v-if="item.vulnerabilitiesCount > 0">
                                <template slot="title">漏洞数量</template>
                                <a-avatar :size="24" :src="'images/folib/bug.svg'" />
                                <span class="mb-0 text-dark bug-count">{{ item.vulnerabilitiesCount }}</span>
                            </a-tooltip>
                            <a-tooltip v-else>
                                <template slot="title">健康</template>
                                <a-avatar :size="24" :src="'images/folib/healthy.svg'" />
                            </a-tooltip>
                        </div>
                    </template>
                    <a-card :bordered="false" class="card-order header-solid mb-24 mx-auto mt-20 mb-50"
                        :bodyStyle="{ paddingTop: 0 }">
                        <template #title>
                            <h6 class="mb-0">{{ item.fileName }}</h6>
                        </template>
                        <a-row :gutter="[24]" type="flex">
                            <a-col :span="24" :md="16">
                                <p class="mb-0">
                                    该依赖含有 <strong>{{ item.evidence.length }}</strong> 个风险凭证，并在扫描检测中发现
                                    <strong>{{ item.vulnerabilitiesCount }}</strong>个漏洞
                                </p>
                                <p class="mb-0">
                                    MD5: <strong>{{ item.md5sum }}</strong>
                                </p>
                                <p class="mb-0">
                                    SHA256: <strong>{{ item.sha256sum }}</strong>
                                </p>
                            </a-col>
                            <a-col :span="24" :md="8" class="ml-auto text-right">
                                <p class="mb-0">
                                    版本号: <strong>{{ item.version }}</strong>
                                </p>

                            </a-col>
                        </a-row>
                        <hr class="gradient-line">

                        <a-row :gutter="[24]" type="flex" class="order-products" align="middle">
                            <a-col :span="24" :md="12">
                                <div class="d-flex">
                                    <a-avatar class="mr-15" :src="'images/folib/' + getImage(item.ecosystem) + '.svg'"
                                        shape="square" :size="80" />
                                    <div>
                                        <h6 class="mb-0 mt-10 font-semibold">{{ item.name }}</h6>
                                        <p class="mb-15">
                                            License: <strong>{{ item.license }}</strong>
                                        </p>
                                        <a-tag class="ant-tag-success font-semibold">{{ item.ecosystem }}</a-tag>
                                    </div>
                                </div>
                            </a-col>
                            <a-col :span="24" :md="12" class="ml-auto text-right">
                                <p>{{ item.description }}</p>
                            </a-col>
                        </a-row>

                        <hr class="gradient-line">

                        <a-row :gutter="[24]" type="flex">
                            <a-col :span="24" :md="24" :lg="24">
                                <a-table :columns="vulnerColumns" :data-source="item.vulnerabilities"
                                    :pagination="false" :row-key="(r, i) => i.toString()">

                                    <a-row slot="expandedRowRender" :gutter="[24, 24]" slot-scope="record">
                                        <a-col :span="24">
                                            <a-card :bordered="false" class="card-billing-info">
                                                <div class="col-info">
                                                    <a-descriptions :title="record.references.length + '个参考信息'"
                                                        :column="1">
                                                        <a-descriptions-item label="说明">
                                                            以下信息均来自于开源社区
                                                        </a-descriptions-item>
                                                        <a-descriptions-item label="相关信息链接">
                                                            <p v-for="(ritem, index1) in record.references"
                                                                :key="index1">
                                                                {{ ritem.url }}
                                                            </p>

                                                        </a-descriptions-item>
                                                    </a-descriptions>
                                                </div>
                                            </a-card>
                                        </a-col>
                                    </a-row>
                                    <template slot="name" slot-scope="text, record">
                                        <div>
                                            <a>
                                                <h6 class="m-0">
                                                    {{ record.name }}
                                                </h6>
                                            </a>
                                        </div>
                                    </template>
                                    <template slot="highestSeverityText" slot-scope="highestSeverityText">
                                        <div class="table-avatar-info">
                                            <a-avatar
                                                v-if="['CRITICAL', 'MEDIUM', 'HIGH', 'LOW'].indexOf(highestSeverityText) != -1"
                                                :size="24"
                                                :src="'images/folib/' + highestSeverityText.toLowerCase() + '.svg'" />
                                            <a-avatar v-else shape="circle" :size="24">{{ highestSeverityText.slice(0,
                                                    1)
                                            }}</a-avatar>
                                            <div class="avatar-info">
                                                <p class="mb-0 text-dark">{{
                                                        highestSeverityText === 'CRITICAL' ? '严重' : highestSeverityText ===
                                                            'MEDIUM' ? '中危' :
                                                            highestSeverityText === 'HIGH' ? '高危' : highestSeverityText ===
                                                                'LOW' ? '低危' :
                                                                highestSeverityText
                                                }}
                                                </p>
                                            </div>
                                        </div>
                                    </template>
                                    <template slot="v2_exploitabilityScore" slot-scope="text, record">{{
                                            record.cvssV2.score
                                    }}</template>
                                    <template slot="v3_exploitabilityScore" slot-scope="text, record">{{
                                            record.cvssV3.baseScore
                                    }}</template>
                                    <template slot="versionStartIncluding" slot-scope="text, record">{{
                                            record.matchedVulnerableSoftware.versionStartIncluding
                                    }}</template>
                                    <template slot="versionEndExcluding" slot-scope="text, record">{{
                                            record.matchedVulnerableSoftware.versionEndExcluding
                                    }}</template>

                                </a-table>
                            </a-col>
                        </a-row>
                    </a-card>
                </a-collapse-panel>
            </a-collapse>
        </a-drawer>
    </div>
</template>

<script>
import store from '@/store'

export default ({
    props: {
        report: {
            type: Array,
            default: () => [],
        },
        visible: {
            type: Boolean,
            default: false,
        }
    },
    created() {
    },
    data() {
        return {

        }
    },
    methods: {
        close() {
            this.$parent.close()
        },
        getImage(ecosystem) {
            return ecosystem ? ecosystem : this.getLayoutTypeHandle()
        },
    }
})
</script>

<style lang="scss" scoped>
$md: 768px;

.vulnerability-report::v-deep {}
</style>