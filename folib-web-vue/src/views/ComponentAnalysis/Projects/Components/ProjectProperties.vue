<template>
    <a-modal
        :title="$t('Projects.ProProperty')"
        :visible="visible"
        :width="900"
        @cancel="handleCancel"
    >
        <div class="by-flex by-row-right">
            <a-input-search
                :placeholder="$t('Projects.SearchPlaceholder')"
                class="by-w-170"
                v-model="searchText"
                @search="handleSearch"
            />
        </div>
        <a-table
            :columns="columns"
            :data-source="tableData"
            rowKey="id"
            :row-selection="{ selectedRowKeys: selectedRowKeys, onChange: onSelectChange }"
        ></a-table>
        <template slot="footer">
            <a-button key="delete" type="danger" ghost @click="handleDelete">
                {{ $t(`Projects.Delete`) }}
            </a-button>
            <a-button key="back" @click="handleCancel">
                {{ $t(`Projects.Cancel`) }}
            </a-button>
            <a-button key="submit" type="primary" @click="createProperty">
                {{ $t(`Projects.CreateProperty`) }}
            </a-button>
        </template>
        <create-property @handle="handleRefresh" ref="create"></create-property>
    </a-modal>
</template>

<script>
import CreateProperty from "./CreateProperty.vue";
import {deleteProjectProperty, getProjectPropertyList} from "@/api/projects";
export default {
    name: "ProjectProperties",
    components: {
        CreateProperty
    },
    data() {
        return {
            visible: false,
            tableData: [],
            columns: [
                {
                    title: () => this.$t('Projects.Group'),
                    dataIndex: 'groupName',
                    ellipsis: true
                },
                {
                    title: this.$t('Projects.Name'),
                    dataIndex: 'propertyName',
                    ellipsis: true
                },
                {
                    title: () => this.$t('Projects.Value'),
                    dataIndex: 'propertyValue',
                    ellipsis: true
                },
                {
                    title: this.$t('Projects.Type'),
                    dataIndex: 'propertyType',
                    ellipsis: true
                },
            ],
            selectedRowKeys: [],
            searchText:'',
            projectUuid: '',
        }
    },
    methods: {
        openModal(uuid) {
            this.visible = true
            this.projectUuid = uuid;
            this.handleSearch();
        },
        createProperty() {
            this.$refs.create.openModal(this.projectUuid)
        },
        handleCancel() {
            this.visible = false
        },
        handleDelete() {
            if (!this.selectedRowKeys || this.selectedRowKeys.length === 0) {
                return;
            }
            const map = this.tableData.reduce((acc, item) => {
                acc.set(item.id, item);
                return acc;
            }, new Map());

            this.selectedRowKeys.forEach((item) => {
                const data = {
                    groupName: map.get(item).groupName,
                    propertyName: map.get(item).propertyName,
                }
                console.log("data", data)
                deleteProjectProperty(this.projectUuid, data).then((res) => {
                    this.loading = true;
                    if (res.status === 200) {
                        this.handleSearch()
                    }
                }).catch(error => {
                    this.$notification.error({
                        message: this.$t("操作失败"),
                        description: "",
                    });
                }).finally(() => {
                    this.loading = false
                });
            })

        },
        onSelectChange(selectedRowKeys) {
            this.selectedRowKeys = selectedRowKeys
        },
        handleSearch() {
            getProjectPropertyList(this.projectUuid, this.searchText).then((res) => {
                this.loading = true;
                if (res.status === 200) {
                    this.tableData = res.data || [];
                }
            }).catch(error => {
                this.$notification.error({
                    message: this.$t("操作失败"),
                    description: "",
                });
            }).finally(() => {
                this.loading = false
            });
        },
        handleRefresh(){
            this.handleSearch();
        }

    }
}
</script>
<style scoped lang="scss">

</style>