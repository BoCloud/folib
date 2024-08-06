<template>
    <div>
        <p class="tip">{{ $t(`Permissions.RepositoriesDesc`) }}</p>
        <a-radio-group v-model="radioModel" :disabled="isView">
            <a-radio-button value="StorageSpace">
                {{ $t('Permissions.StorageSpace') }}
            </a-radio-button>
            <a-radio-button value="Repositories">
                {{ $t('Permissions.Repositories') }}
            </a-radio-button>
        </a-radio-group>
        <a-steps v-model="step" type="navigation" size="small" class="step" :style="{width: radioModel === 'StorageSpace' ? '49.5%' : '100%'}">
            <a-step
                :title="$t(`Permissions.${ radioModel === 'StorageSpace' ? 'SelectStorageSpace' : 'SelectRepositories'}`)"
                :status="step === 0 ? 'process' : 'wait'"
                :description="$t(`Permissions.${radioModel === 'StorageSpace' ? 'SelectStorageSpaceDesc' : 'SelectRepositoriesDesc'}`)"
            />
            <a-step
                v-if="radioModel === 'Repositories'"
                :title="$t('Permissions.SetPatterns')"
                :status="step === 1 ? 'process' : 'wait'"
                :disabled="!selectedRowKeys.length"
                :description="$t('Permissions.RepositoriesPatternsDesc')"
            />
        </a-steps>
        <a-table
            v-if="!step"
            :row-selection="{ selectedRowKeys: selectedRowKeys, onChange: onSelectChange, getCheckboxProps: getCheckboxProps }"
            :columns="tableColumns"
            :data-source="tableData"
            size="small"
            :pagination="false"
            :scroll="{ y: 500 }"
        />
        <div v-else>
            <div class="by-flex by-row-right by-m-b-10">
                <a-input
                    v-model="searchText"
                    :placeholder="$t('Permissions.Search')"
                    allow-clear
                    :style="{width: searchText || searchFocus ? '404px' : '200px'}"
                    @focus="searchFocus = true"
                    @blur="searchFocus = false"
                >
                    <a-icon slot="prefix" type="search" />
                </a-input>
            </div>
            <a-table
                :columns="i18nPerRepositoryColumns"
                :data-source="filterPerRepositoryList"
                :row-key="(r, i) => i.toString()"
                :pagination="false"
                :scroll="{ y: 300 }"
                class="table-custom"
            >
                <div slot="includes" slot-scope="text, record">
                    <div v-if="!isView" class="insert-item" :class="{ 'has-error': record.isInError}">
                        <a-input
                            v-model="record.currentInPattern"
                            :placeholder="$t('Permissions.NewPatterns')"
                            @focus="record.isInError = false"
                            @blur="handleAddRowInPattern(record)"
                        />
                        <a-icon
                            type="plus"
                            class="per-plus-icon"
                            @click="handleAddRowInPattern(record)"
                        />
                    </div>
                    <div v-if="record.isInError" class="err-tip">{{ $t('Permissions.ErrPatterns') }}</div>
                    <div class="insert-content per-insert-content">
                        <div
                            v-for="(item, index) in record.includes"
                            :key="index"
                            class="by-m-t-10 single-pattern-item"
                        >
                            {{ item }}
                            <a-icon
                                v-if="!isView"
                                type="close"
                                class="close-icon"
                                @click="record.includes.splice(index, 1)"
                            />
                        </div>
                    </div>
                </div>
            </a-table>
        </div>
    </div>
</template>

<script>
export default {
    name: "repositories",
    props: {
        repositoriesList: {
            type: Array,
            default: () => []
        },
        storageList: {
            type: Array,
            default: () => []
        },
        isView: {
            type: Boolean,
            default: false
        },
    },
    data()
    {
        return {
            step: 0,
            tableData: [],
            selectedRowKeys: [],
            radioModel: 'StorageSpace',
            currentInPattern: '',
            form: this.$form.createForm(this, { name: 'repositories' }),
            searchText: '',
            searchFocus: false,
            perRepositoryColumns: [
                {
                    title: '名称',
                    i18nKey: 'Permissions.SingleName',
                    dataIndex: 'title',
                    key: 'title',
                    width: '350px',
                },
                {
                    title: '包含路径',
                    i18nKey: 'Permissions.IncludePatterns',
                    dataIndex: 'includes',
                    key: 'includes',
                    scopedSlots: { customRender: 'includes' },
                },
            ],
            perRepositoryList: []
        }
    },
    computed: {
        i18nPerRepositoryColumns () {
            return this.perRepositoryColumns.map(column => {
                if (column.i18nKey) {
                    column.title = this.$t(column.i18nKey);
                }
                return column;
            })
        },
        filterPerRepositoryList() {
            return this.perRepositoryList.filter(item => {
                return item.title.indexOf(this.searchText) !== -1
            })
        },
        tableColumns() {
            return [ { dataIndex: 'title', title: this.$t('Permissions.SingleName') } ]
        }
    },
    watch: {
        selectedRowKeys: {
            handler(newVal) {
                this.perRepositoryList = newVal.map(item => {
                    return {
                        title: item,
                        includes: [],
                        currentInPattern: '',
                        isInError: false,
                    }
                })
            },
            deep: true
        },
        storageList: {
            handler(newVal) {
                if (this.radioModel !== 'StorageSpace') return
                this.tableData = newVal.map(item => {
                    return {
                        key: item.id,
                        title: item.id,
                    }
                })
            },
            deep: true
        },
        repositoriesList: {
            handler(newVal) {
                if (this.radioModel === 'StorageSpace') return
                this.tableData = newVal.map(item => {
                    return {
                        key: item.key,
                        title: item.key,
                    }
                })
            },
            deep: true
        },
        radioModel(val) {
            if (val === 'StorageSpace') {
                this.tableData = this.storageList.map(item => {
                    return {
                        key: item.id,
                        title: item.id,
                    }
                })
            } else {
                this.tableData = this.repositoriesList.map(item => {
                    return {
                        key: item.key,
                        title: item.key,
                    }
                })
            }
            this.step = 0
            this.selectedRowKeys = []
        }
    },
    methods: {
        init() {
            this.step = 0
            this.radioModel = 'StorageSpace'
            this.selectedRowKeys = []
        },
        onSelectChange(selectedRowKeys) {
            this.selectedRowKeys = selectedRowKeys;
        },
        getCheckboxProps() {
            return {
                props: {
                    disabled: this.isView
                }
            }
        },
        handleAddRowInPattern(record) {
            if (record.currentInPattern.trim()) {
                if (record.includes.indexOf(record.currentInPattern) === -1) {
                    record.includes.unshift(record.currentInPattern)
                    record.currentInPattern = ''
                } else {
                    record.isInError = true
                }
            }
        },
        setPath(res) {
            this.step = 1
            this.perRepositoryList.forEach(item => {
                res.forEach(ele => {
                    if (item.title === `${ele.storageId}/${ele.repositoryId}`) {
                        item.includes.push(ele.path)
                    }
                })
            })
        },
        getResources() {
            if (this.radioModel === 'StorageSpace') {
                return this.selectedRowKeys.map(item => {
                    return {
                        storageId: item
                    }
                })
            }
            if (this.perRepositoryList.some(item => item.includes.length)) {
                 const list = []
                 this.perRepositoryList.forEach(item => {
                     if (item.includes.length) {
                         item.includes.forEach(ele => {
                             list.push({
                                 storageId: item.title.split('/')[0],
                                 repositoryId: item.title.split('/')[1],
                                 path: ele
                             })
                         })
                     } else {
                         list.push({
                             storageId: item.title.split('/')[0],
                             repositoryId: item.title.split('/')[1],
                         })
                     }
                })
                return list
            }
            return this.selectedRowKeys.map(item => {
                return {
                    storageId: item.split('/')[0],
                    repositoryId: item.split('/')[1],
                }
            })
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
.per-insert-content {
    max-height: 150px;
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

.err-tip {
    color: #f5222d;
}

.table-custom{
    ::v-deep .ant-table-body .ant-table-tbody {
        tr td {
            vertical-align: top;
        }
    }
}
</style>