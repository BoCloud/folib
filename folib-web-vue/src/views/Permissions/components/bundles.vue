<template>
    <a-modal
        v-model="visible"
        :title="$t(`Permissions.${isEdit ? 'EditReleaseBundles' : 'AddReleaseBundles'}`)"
        width="50vw"
        @close="closeModal"
    >
        <p class="tip">{{ $t(`Permissions.BundleTip`) }}</p>
        <a-steps v-model="step" type="navigation" size="small" class="step">
            <a-step
                :title="$t('Permissions.SelectBundle')"
                :status="step === 0 ? 'process' : 'wait'"
                :description="$t('Permissions.SelectBundleDesc')"
            />
            <a-step
                :title="$t('Permissions.SetPatterns')"
                :status="step === 1 ? 'process' : 'wait'"
                :disabled="!targetKeys.length"
                :description="$t('Permissions.BundlePatternsDesc')"
            />
        </a-steps>
        <a-transfer
            v-if="!step"
            :data-source="mockData"
            :target-keys="targetKeys"
            :show-search="true"
            :filter-option="(inputValue, item) => item.title.indexOf(inputValue) !== -1"
            :show-select-all="false"
            @change="onChange"
        >
            <template
                slot="children"
                slot-scope="{
                    props: { direction, filteredItems, selectedKeys, disabled: listDisabled },
                    on: { itemSelectAll, itemSelect },
                }"
            >
                <a-table
                    :row-selection="getRowSelection({ disabled: listDisabled, selectedKeys, itemSelectAll, itemSelect })"
                    :columns="direction === 'left' ? leftTableColumns : rightTableColumns"
                    :data-source="filteredItems"
                    size="small"
                    :pagination="false"
                    :style="{ pointerEvents: listDisabled ? 'none' : null }"
                    :custom-row="
                        ({ key, disabled: itemDisabled }) => ({
                            on: {
                                click: () => {
                                    if (itemDisabled || listDisabled) return;
                                    itemSelect(key, !selectedKeys.includes(key));
                                },
                            },
                        })
                    "
                />
            </template>
        </a-transfer>
        <div v-else>
            <a-radio-group v-model="radioModel">
                <a-radio-button value="ALL">
                    {{ $t('Permissions.AllBundles') }}
                </a-radio-button>
                <a-radio-button value="PER">
                    {{ $t('Permissions.PerBundle') }}
                </a-radio-button>
            </a-radio-group>
            <p class="optional-tip">{{ $t('Permissions.OptionalTip') }}</p>
            <div v-if="radioModel === 'ALL'" class="patterns by-flex by-col-stretch" >
                <div class="pattern-item by-flex-1">
                    <div class="by-flex by-row-between">
                        <div class="pattern-title">{{ $t('Permissions.IncludePatterns') }}</div>
                        <a-tooltip placement="topLeft" :title="$t('Permissions.PatternsTip')">
                            <a-icon type="question-circle" />
                        </a-tooltip>
                    </div>
                    <div class="by-m-t-10">
                        <div class="insert-item">
                            <a-form :form="form">
                                <a-form-item>
                                    <a-input
                                        v-decorator="['currentInPattern']"
                                        :placeholder="$t('Permissions.NewPatterns')"
                                        @blur="handleAddInPattern"
                                    />
                                    <a-icon
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
                        <a-tooltip placement="topLeft" :title="$t('Permissions.PatternsTip')">
                            <a-icon type="question-circle" />
                        </a-tooltip>
                    </div>
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
                    :columns="i18nPerBundlesColumns"
                    :data-source="filterPerBundlesList"
                    :row-key="(r, i) => i.toString()"
                    :pagination="false"
                    :scroll="{ y: 300 }"
                    class="table-custom"
                >
                    <div slot="includes" slot-scope="text, record">
                        <div class="insert-item" :class="{ 'has-error': record.isInError}">
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
                                    type="close"
                                    class="close-icon"
                                    @click="record.includes.splice(index, 1)"
                                />
                            </div>
                        </div>
                    </div>
                    <div slot="excludes" slot-scope="text, record">
                        <div class="insert-item" :class="{ 'has-error': record.isExError}">
                            <a-input
                                v-model="record.currentExPattern"
                                :placeholder="$t('Permissions.NewPatterns')"
                                @focus="record.isExError = false"
                                @blur="handleAddRowExPattern(record)"
                            />
                            <a-icon
                                type="plus"
                                class="per-plus-icon"
                                @click="handleAddRowExPattern(record)"
                            />
                        </div>
                        <div v-if="record.isExError" class="err-tip">{{ $t('Permissions.ErrPatterns') }}</div>
                        <div class="insert-content per-insert-content">
                            <div
                                v-for="(item, index) in record.excludes"
                                :key="index"
                                class="by-m-t-10 single-pattern-item"
                            >
                                {{ item }}
                                <a-icon
                                    type="close"
                                    class="close-icon"
                                    @click="record.excludes.splice(index, 1)"
                                />
                            </div>
                        </div>
                    </div>
                </a-table>
            </div>
        </div>
    </a-modal>
</template>

<script>
import { difference, cloneDeep } from "lodash";

export default {
    name: "bundles",
    data()
    {
        return {
            visible: false,
            isEdit: false,
            step: 0,
            mockData: [
                {
                    key: '1',
                    title: 'Bundles 1',
                },
            ],
            targetKeys: [],
            targetData: [],
            leftTableColumns: [
                {
                    dataIndex: 'title',
                    title: 'Name',
                },
            ],
            rightTableColumns: [
                {
                    dataIndex: 'title',
                    title: 'Name',
                },
            ],
            radioModel: 'ALL',
            currentInPattern: '',
            includePatterns: ['**'],
            form: this.$form.createForm(this, { name: 'bundles' }),
            currentExPattern: '',
            excludePatterns: [],
            exForm: this.$form.createForm(this, { name: 'bundlesEx' }),
            searchText: '',
            searchFocus: false,
            perBundlesColumns: [
                {
                    title: '名称',
                    i18nKey: 'Permissions.SingleName',
                    dataIndex: 'name',
                    key: 'name',
                    width: '150px',
                },
                {
                    title: '包含规则',
                    i18nKey: 'Permissions.IncludePatterns',
                    dataIndex: 'includes',
                    key: 'includes',
                    scopedSlots: { customRender: 'includes' },
                },
                {
                    title: '排除规则',
                    i18nKey: 'Permissions.ExcludePatterns',
                    dataIndex: 'excludes',
                    key: 'excludes',
                    scopedSlots: { customRender: 'excludes' },
                },
            ],
            perBundlesList: []
        }
    },
    computed: {
        i18nPerBundlesColumns () {
            return this.perBundlesColumns.map(column => {
                if (column.i18nKey) {
                    column.title = this.$t(column.i18nKey);
                }
                return column;
            })
        },
        filterPerBundlesList() {
            return this.perBundlesList.filter(item => {
                return item.name.indexOf(this.searchText) !== -1
            })
        }
    },
    watch: {
        targetData: {
            handler(newVal) {
                this.perBundlesList = newVal.map(item => {
                    return {
                        name: item.title,
                        includes: cloneDeep(this.includePatterns),
                        excludes: cloneDeep(this.excludePatterns),
                        currentInPattern: '',
                        currentExPattern: '',
                        isInError: false,
                        isExError: false,
                    }
                })
            },
            deep: true
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
        onChange(nextTargetKeys) {
            this.targetKeys = nextTargetKeys;
            this.targetData = this.mockData.filter(
                item => nextTargetKeys.indexOf(item.key) !== -1
            );
        },
        getRowSelection({ disabled, selectedKeys, itemSelectAll, itemSelect }) {
            return {
                getCheckboxProps: item => ({ props: { disabled: disabled || item.disabled } }),
                onSelectAll(selected, selectedRows) {
                    const treeSelectedKeys = selectedRows
                        .filter(item => !item.disabled)
                        .map(({ key }) => key);
                    const diffKeys = selected
                        ? difference(treeSelectedKeys, selectedKeys)
                        : difference(selectedKeys, treeSelectedKeys);
                    itemSelectAll(diffKeys, selected);
                },
                onSelect({ key }, selected) {
                    itemSelect(key, selected);
                },
                selectedRowKeys: selectedKeys,
            };
        },
        handleAddInPattern()
        {
            this.form.validateFields((err, values) => {
                if (values['currentInPattern'].trim()) {
                    if (this.includePatterns.indexOf(values['currentInPattern']) === -1) {
                        this.includePatterns.unshift(values['currentInPattern'])
                        this.perBundlesList.forEach(item => {
                            if (item.includes.indexOf(values['currentInPattern']) === -1)
                                item.includes.unshift(values['currentInPattern'])
                        })
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
                        this.perBundlesList.forEach(item => {
                            if (item.excludes.indexOf(values['currentExPattern']) === -1)
                                item.excludes.unshift(values['currentExPattern'])
                        })
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
        handleAddRowExPattern(record) {
            if (record.currentExPattern.trim()) {
                if (record.excludes.indexOf(record.currentExPattern) === -1) {
                    record.excludes.unshift(record.currentExPattern)
                    record.currentExPattern = ''
                } else {
                    record.isExError = true
                }
            }
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