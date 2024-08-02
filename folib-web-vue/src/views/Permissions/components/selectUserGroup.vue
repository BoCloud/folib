<template>
    <a-modal
        v-model="visible"
        :title="$t(`Permissions.${type === 'USER' ? 'SelectUsers' : 'SelectGroups'}`)"
        width="50vw"
        @close="closeModal"
    >
        <a-transfer
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
    </a-modal>
</template>

<script>
import { difference } from "lodash";

export default {
    name: "selectUserGroup",
    data()
    {
        return {
            visible: false,
            type: '',
            mockData: [
                {
                    key: '1',
                    title: 'User 1',
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
        }
    },
    methods: {
        openModal(type = 'USER')
        {
            this.type = type
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
    }
}
</script>

<style scoped lang="scss">

</style>