<template>
    <a-modal
        v-model="visible"
        :title="$t(`Permissions.${isEdit ? 'EditBuilds' : 'AddBuilds'}`)"
        width="50vw"
        @close="closeModal"
    >
        <p class="tip">{{ $t(`Permissions.BuildsDesc`) }}</p>
        <a-radio-group v-model="radioModel">
            <a-radio-button value="NAME">
                {{ $t('Permissions.ByBuildName') }}
            </a-radio-button>
            <a-radio-button value="PATTERN">
                {{ $t('Permissions.ByPattern') }}
            </a-radio-button>
        </a-radio-group>
        <p class="optional-tip">{{ $t('Permissions.BuildOptionTip') }}</p>
        <div v-if="radioModel === 'NAME'">
            <a-tabs v-model="tabKey">
                <a-tab-pane key="all" :tab="$t('Permissions.All')"></a-tab-pane>
                <a-tab-pane key="selected" :tab="`${$t('Permissions.Selected')}(${selectedCount})`"></a-tab-pane>
                <div slot="tabBarExtraContent">
                    <a-input v-model="searchText" :placeholder="$t('Permissions.Search')" allow-clear size="small">
                        <a-icon slot="prefix" type="search" />
                    </a-input>
                </div>
            </a-tabs>
            <a-table
                :row-selection="{ selectedRowKeys: selectedRowKeys, onChange: onSelectChange }"
                :columns="columns"
                :data-source="filterDataSource"
                :pagination="false"
                :scroll="{ y: 300 }"
            />
        </div>
        <div v-else>
            <div class="patterns by-flex by-col-stretch" >
                <div class="pattern-item by-flex-1">
                    <div class="by-flex by-row-between">
                        <div class="pattern-title">{{ $t('Permissions.IncludePatterns') }}</div>
                        <a-tooltip placement="topLeft" :title="$t('Permissions.BundlesPatternsTip')">
                            <a-icon type="question-circle" />
                        </a-tooltip>
                    </div>
                    <a-popconfirm
                        :title="$t('Permissions.CheckConfirmTip')"
                        :visible="confirmVisible"
                        placement="topLeft"
                        @visibleChange="handleVisibleChange"
                        @confirm="confirm"
                    >
                        <a-checkbox :checked="allBuild" class="by-m-t-10">
                            <a-tooltip placement="topLeft" :title="$t('Permissions.CheckTip')">
                                {{ $t('Permissions.IncludeAllBuild') }}
                            </a-tooltip>
                        </a-checkbox>
                    </a-popconfirm>
                    <div class="by-m-t-10">
                        <div class="insert-item">
                            <a-form :form="form">
                                <a-form-item>
                                    <a-input
                                        v-decorator="['currentInPattern']"
                                        :placeholder="$t('Permissions.NewPatterns')"
                                        :disabled="allBuild"
                                        @blur="handleAddInPattern"
                                    />
                                    <a-icon
                                        v-if="!allBuild"
                                        type="plus"
                                        class="plus-icon"
                                        @click="handleAddInPattern"
                                    />
                                </a-form-item>
                            </a-form>
                        </div>
                        <div class="insert-content">
                            <div
                                v-for="(item, index) in includePatterns"
                                :key="index"
                                class="by-m-t-10 single-pattern-item"
                            >
                                {{ item }}
                                <a-icon
                                    v-if="!allBuild"
                                    type="close"
                                    class="close-icon"
                                    @click="includePatterns.splice(index, 1)"
                                />
                            </div>
                        </div>
                    </div>
                </div>
                <div class="pattern-item by-m-l-20 by-flex-1">
                    <div class="by-flex by-row-between">
                        <div class="pattern-title">{{ $t('Permissions.IncludePatterns') }}</div>
                        <a-tooltip placement="topLeft" :title="$t('Permissions.BundlesPatternsTip')">
                            <a-icon type="question-circle" />
                        </a-tooltip>
                    </div>
                    <div class="by-p-t-30"></div>
                    <div class="by-m-t-10">
                        <div class="insert-item">
                            <a-form :form="exForm">
                                <a-form-item>
                                    <a-input
                                        v-decorator="['currentExPattern']"
                                        :placeholder="$t('Permissions.NewPatterns')"
                                        @blur="handleAddExPattern"
                                    />
                                    <a-icon
                                        type="plus"
                                        class="plus-icon"
                                        @click="handleAddExPattern"
                                    />
                                </a-form-item>
                            </a-form>
                        </div>
                        <div class="insert-content">
                            <div
                                v-for="(item, index) in excludePatterns"
                                :key="index"
                                class="by-m-t-10 single-pattern-item"
                            >
                                {{ item }}
                                <a-icon
                                    type="close"
                                    class="close-icon"
                                    @click="excludePatterns.splice(index, 1)"
                                />
                            </div>
                        </div>
                    </div>
                </div>
            </div>
            <a-divider orientation="left">
                {{ $t('Permissions.Preview') }}
            </a-divider>
            <div class="by-flex by-row-between by-m-b-20">
                <span>{{$t('Permissions.TotalBuilds')}}:{{totalBuilds}}</span>
                <a-input
                    v-model="searchPatternText"
                    :placeholder="$t('Permissions.Search')"
                    allow-clear
                    :style="{width: searchPatternText || searchFocus ? '404px' : '200px'}"
                    @focus="searchFocus = true"
                    @blur="searchFocus = false"
                >
                    <a-icon slot="prefix" type="search" />
                </a-input>
            </div>
            <a-table
                :row-selection="{ selectedRowKeys: preSelectedRowKeys, onChange: onPreSelectChange }"
                :columns="columns"
                :data-source="filterPreSource"
                :pagination="false"
                :scroll="{ y: 300 }"
            />
        </div>
    </a-modal>
</template>

<script>
export default {
    name: "builds",
    data()
    {
        return {
            visible: false,
            isEdit: false,
            radioModel: 'NAME',
            searchText: '',
            tabKey: 'all',
            columns: [
                {
                    title: 'Build Name',
                    dataIndex: 'name',
                }
            ],
            dataSource: [
                {
                    name: 'test1'
                },
                {
                    name: 'test2'
                },
                {
                    name: 'test3'
                },
            ],
            selectedRowKeys: [],
            currentInPattern: '',
            includePatterns: ['**'],
            form: this.$form.createForm(this, { name: 'builds' }),
            currentExPattern: '',
            excludePatterns: [],
            exForm: this.$form.createForm(this, { name: 'buildsEx' }),
            searchPatternText: '',
            searchFocus: false,
            confirmVisible: false,
            allBuild: false,
            totalBuilds: 0,
            preSource: [],
            preSelectedRowKeys: [],
        }
    },
    computed: {
        selectedCount() {
            return this.selectedRowKeys.length
        },
        filterDataSource() {
            return this.dataSource.filter((item, index) => {
                if (this.tabKey === 'all')
                    return item.name.indexOf(this.searchText) !== -1
                return item.name.indexOf(this.searchText) !== -1 && this.selectedRowKeys.indexOf(index) !== -1
            })
        },
        filterPreSource() {
            return this.preSource.filter(item => {
                return item.name.indexOf(this.searchText) !== -1
            })
        }
    },
    watch: {
        allBuild(val){

        }
    },
    methods: {
        openModal()
        {
            this.visible = true
        },
        closeModal()
        {
            this.visible = false
        },
        onSelectChange(selectedRowKeys) {
            this.selectedRowKeys = selectedRowKeys;
        },
        handleAddInPattern()
        {
            this.form.validateFields((err, values) => {
                if (values['currentInPattern'].trim()) {
                    if (this.includePatterns.indexOf(values['currentInPattern']) === -1) {
                        this.includePatterns.unshift(values['currentInPattern'])
                        this.form.resetFields()
                    } else {
                        this.form.setFields({
                            currentInPattern: {
                                value: values['currentInPattern'],
                                errors: [new Error(this.$t('Permissions.ErrPatterns'))],
                            },
                        })
                    }
                }
            });
        },
        handleAddExPattern()
        {
            this.exForm.validateFields((err, values) => {
                if (values['currentExPattern'].trim()) {
                    if (this.excludePatterns.indexOf(values['currentExPattern']) === -1) {
                        this.excludePatterns.unshift(values['currentExPattern'])
                        this.exForm.resetFields()
                    } else {
                        this.exForm.setFields({
                            currentExPattern: {
                                value: values['currentExPattern'],
                                errors: [new Error(this.$t('Permissions.ErrPatterns'))],
                            },
                        })
                    }
                }
            });
        },
        confirm() {
            this.allBuild = true
            this.includePatterns = ['**']
        },
        handleVisibleChange(visible) {
            if (!visible) {
                this.confirmVisible = false;
                return;
            }
            if (this.allBuild) {
                this.allBuild = false
                return
            }
            if (this.includePatterns.length > 1 || (this.includePatterns.length === 1 && this.includePatterns[0] !== '**')){
                this.confirmVisible = true
            } else {
                this.confirmVisible = false;
                this.confirm()
            }
        },
        onPreSelectChange(selectedRowKeys){
            this.preSelectedRowKeys = selectedRowKeys;
        }
    }
}
</script>

<style scoped lang="scss">
.optional-tip {
    font-size: 12px;
    line-height: 1.5;
    margin-top: 14px;
    color: #999db4;
}
.insert-content {
    box-sizing: content-box;
    max-height: 200px;
    overflow: auto;
}
.insert-item {
    position: relative;

    ::v-deep .ant-form-item {
        margin-bottom: 0;
    }

    .plus-icon {
        position: absolute;
        right: 12px;
        top: 2px;
    }

    .per-plus-icon {
        position: absolute;
        right: 12px;
        top: 12px;
    }
}
.single-pattern-item {
    background: #f8fafb;
    color: #999db4;
    border: solid 1px #c9d0e3;
    padding-right: 40px;
    padding-left: 12px;
    line-height: 40px;
    border-radius: 6px;
    position: relative;

    .close-icon {
        position: absolute;
        right: 12px;
        top: 12px;
    }
}
</style>