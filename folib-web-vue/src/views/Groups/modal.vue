<template>
    <a-drawer
        placement="right"
        width="50%"
        :title="(isView ? $t('Groups.View') : isEdit ? $t('Groups.Edit') : $t('Groups.Create'))"
        :visible="visible"
        @close="closeModal"
    >
        <a-spin :spinning="spinning">
            <div class="by-p-b-60">
                <span class="by-f-w-600" style="font-size: 18px; color: #333; margin-bottom: 16px; display: block;">
                    {{ $t('Groups.GroupSettings') }}
                </span>
                <div class="by-p-l-0">
                    <a-form-model
                        ref="ruleForm"
                        :model="form"
                        :rules="rules"
                        :label-col="labelCol"
                        :wrapper-col="wrapperCol"

                    >
                        <a-form-model-item ref="name" :label="$t('Groups.GroupName')" prop="name">
                            <a-input class="by-w-300"
                                v-model="form.name"
                                :disabled="isView"
                                @blur="() => { $refs.name.onFieldBlur() }"
                            />
                        </a-form-model-item>
                        <a-form-model-item ref="name" :label="$t('Groups.Description')" prop="description">
                            <a-input 
                                v-model="form.description"
                                :disabled="isView"
                                class="by-w-300 description-input"
                            />
                        </a-form-model-item>
                    </a-form-model>
                    <!-- <a-divider orientation="left">
                        {{ $t('Groups.Options') }}
                    </a-divider>-->
                    <a-checkbox v-model="auto" :disabled="isView" class="by-m-t-10">
                        {{ $t('Groups.Automatically') }}
                    </a-checkbox>
                </div>
                <span class="by-f-w-600 by-m-t-40" style="font-size: 18px; color: #333; margin-bottom: 16px; display: block;">
                    {{ $t('Groups.GroupUser') }}
                </span>
                           
                <div class="transfer-container">
                    <a-transfer
                        ref="leftList"
                        :data-source="allUsers"
                        :target-keys="selectedUserKeys"
                        :disabled="isView"
                        :render="item => item.title"
                        @change="handleTransferChange"
                        :list-style="{
                            width: '300px',
                            height: '400px',
                        }"
                        :titles="[$t('Groups.transfer.titles[0]'), $t('Groups.transfer.titles[1]')]"
                        :locale="{
                            itemUnit: $t('Groups.transfer.itemUnit'),
                            itemsUnit: $t('Groups.transfer.itemsUnit'),
                            searchPlaceholder: $t('Groups.transfer.searchPlaceholder'),
                        }"
                        :show-search="true"
                        :filter-option="handleFilter"
                        @scroll="handleScroll"
                        @search="handleLeftSearch"
                    >
                        <!-- <span
                            slot="header"
                            slot-scope="{ direction, filteredItems, checkedKeys, onItemSelectAll, onItemSelect }"
                            :style="{ display: 'inline-block', width: '100%' }"
                        >
                            <a-input
                                v-if="direction === 'left'"
                                v-model="leftSearchValue"
                                :placeholder="$t('Groups.transfer.searchPlaceholder')"
                                class="custom-transfer-search"
                                @change="handleLeftSearch"
                            >
                                <a-icon slot="prefix" type="search" />
                            </a-input>
                        </span> -->
                    </a-transfer>
                </div>
                <!-- <a-divider orientation="left">
                    <span class="by-f-w-600">{{ $t('Groups.Users') }}</span>
                </a-divider> -->
                <!-- <div class="by-flex by-row-right by-m-b-10">
                    <a-input-search v-model="userSearchText" size="small" :placeholder="$t('Groups.EnterTheNameQuery')" @search="handleSearch()" class="by-w-200"/>
                </div> -->
                <!-- <a-table
                    :row-selection="{ selectedRowKeys: selectedRowKeys, onChange: onSelectChange, getCheckboxProps: getCheckboxProps }"
                    :columns="tableColumns"
                    :data-source="tableData"
                    :pagination="{ pageSize: limit, current: page, total: total, showLessItems: true }"
                    :loading="loading"
                    @change="handleChangeTable"
                    :scroll="{ y: 500 }"
                />
                <template v-if="isEdit">
                    <a-divider orientation="left">
                        <span class="by-f-w-600">{{ $t('Groups.GroupPermissions') }}</span>
                    </a-divider>
                    <a-table
                        :columns="i18nPermissionColumns"
                        :data-source="permissionsList"
                        :row-key="(r, i) => i.toString()"
                        :pagination="false"
                        :scroll="{ y: 300 }"
                    >
                        <div slot="customTag" slot-scope="status">
                            <div class="join-status" v-if="status">
                                <a-icon type="check" />
                            </div>
                        </div>
                    </a-table>
                </template> -->
            </div>
        </a-spin>
        <div v-if="!isView" class="drawer-footer">
            <a-button :style="{ marginRight: '8px' }" @click="closeModal">
                {{ $t(`Permissions.Cancel`) }}
            </a-button>
            <a-button type="primary" :loading="confirmLoading" @click="handleConfirm">
                {{ $t(`Permissions.Confirm`) }}
            </a-button>
        </div>
    </a-drawer>
</template>

<script>
import { queryUser } from "@/api/users";
import { getGroupDetail, createGroup, updateGroup } from "@/api/group";

export default {
    name: "modal",
    data() {
        return {
            visible: false,
            spinning: false,
            isView: false,
            isEdit: false,
            loading: false,
            confirmLoading: false,
            form: {
                id: '',
                name: '',
                description: '',
            },
            labelCol: { span: 24 },
            wrapperCol: { span: 24 },
            admin: false,
            resources: false,
            resourcesDisabled: false,
            auto: true,
            autoDisabled: false,
            page: 1,
            limit: 20, // 每次加载的用户数量
            hasMore: true, // 是否还有更多数据
            total: 0,
            tableData: [],
            selectedRowKeys: [],
            userSearchText: '',
            searchText: '',
            allUsers:[],
            selectedUserKeys:[],
            permissionsColumns: [
                {
                    title: '权限名称',
                    i18nKey: 'Groups.PermissionsName',
                    dataIndex: 'name',
                    key: 'name',
                    width: 200,
                    scopedSlots: { customRender: 'name' },
                },
                {
                    title: '下载',
                    i18nKey: 'Groups.Download',
                    dataIndex: 'download',
                    key: 'download',
                    scopedSlots: { customRender: 'customTag' },
                },
                {
                    title: '部署/缓存',
                    i18nKey: 'Groups.DeployCache',
                    dataIndex: 'deployCache',
                    key: 'deployCache',
                    scopedSlots: { customRender: 'customTag' },
                },
                {
                    title: '删除/更新',
                    i18nKey: 'Groups.DeleteUpdate',
                    dataIndex: 'deleteUpdate',
                    key: 'deleteUpdate',
                    scopedSlots: { customRender: 'customTag' },
                },
                {
                    title: '晋级/分发',
                    i18nKey: 'Groups.PromoDistribution',
                    dataIndex: 'promoDistribution',
                    key: 'promoDistribution',
                    scopedSlots: { customRender: 'customTag' },
                },
            ],
            sourceData: [],
            leftColumns: [{ dataIndex: 'title', title: this.$t('Groups.AllUsers') }],
            rightColumns: [{ dataIndex: 'title', title: this.$t('Groups.SelectedUsers') }],
            leftSearchValue: '',
        }
    },
    computed: {
        rules() {
            return {
                name: [
                    { required: true, message: this.$t('Groups.NameRequired'), trigger: 'blur' },
                ]
            }
        },
        i18nPermissionColumns () {
            return this.permissionsColumns.map(column => {
                if (column.i18nKey) {
                    column.title = this.$t(column.i18nKey);
                }
                return column;
            })
        },
        permissionsList() {
            return this.sourceData.filter(item => {
                return item.name.indexOf(this.searchText) !== -1;
            })
        },
        tableColumns() {
            return [ { dataIndex: 'title', title: this.$t('Permissions.SingleName') } ]
        },
    },
    watch: {
        admin(val) {
            if (val) {
                this.resources = true;
                this.auto = false;
                this.resourcesDisabled = true;
                this.autoDisabled = true;
            } else {
                this.resourcesDisabled = false;
                this.autoDisabled = false;
            }
        }
    },
    methods: {
        openModal(id, isView) {
            this.visible = true;
            this.isView = isView;
            this.isEdit = !!id;
            this.resetData();
            this.getUsers();
            if (id) this.getDetail(id);
        },
        closeModal() {
            this.page=1
            this.$refs.ruleForm.resetFields()
            this.auto = true
            this.selectedRowKeys = []
            this.visible = false;
        },
        onChange(nextTargetKeys) {
            this.targetKeys = nextTargetKeys;
            this.targetData = this.mockData.filter(
                item => nextTargetKeys.indexOf(item.key) !== -1
            );
        },
        getDetail(id)
        {
            this.spinning = true
            getGroupDetail(id).then(res => {
                const { roleAccess, userGroupDTO } = res
                this.form.id = id
                this.form.name = userGroupDTO.groupName
                this.form.description = userGroupDTO.description
                this.selectedUserKeys = userGroupDTO.userIds
                this.auto = userGroupDTO.joinGroup === '1'
                this.sourceData = []
                for (const key in roleAccess) {
                    this.sourceData.push({
                        name: key,
                        download: roleAccess[key].includes('ARTIFACTS_RESOLVE'),
                        deployCache: roleAccess[key].includes('ARTIFACTS_DEPLOY'),
                        promoDistribution: roleAccess[key].includes('ARTIFACTS_PROMOTION'),
                        deleteUpdate: roleAccess[key].includes('ARTIFACTS_DELETE')
                    })
                }
            }).finally(() => {
                this.spinning = false
            })
        },
        handleSearch() {
            this.page = 1
            this.getUsers()
        },
        handleChangeTable(pagination) {
            if (pagination) this.page = pagination.current
            this.getUsers()
        },
       
        handleTransferChange(nextTargetKeys, direction, moveKeys) {
            this.selectedUserKeys = nextTargetKeys;

            const leftListCount = this.allUsers.length - nextTargetKeys.length;
            const threshold = Math.min(this.limit, 10);

            if (leftListCount <= threshold && this.hasMore && !this.loading) {
                this.$nextTick(() => {
                    this.getUsers(true);
                });
            }
        },
        getUsers(isLoadMore = false) {
            if (!isLoadMore) {
                this.page = 1;
                this.allUsers = [];
                this.hasMore = true;
            }
            if (!this.hasMore || this.loading) {
                return;
            }

            this.loading = true;
            queryUser({username: this.leftSearchValue}, {page: this.page, limit: this.limit}).then(res => {
                const newUsers = res.data.rows.map((item) => ({
                    key: item.username,
                    title: item.username,
                }));
                this.allUsers = [...this.allUsers, ...newUsers];
                this.total = res.data.total;
                this.hasMore = this.allUsers.length < this.total;
                this.page++;
            }).catch(error => {
                console.error('Error fetching users:', error);
            }).finally(() => {
                this.loading = false;
            });
        },
        handleScroll(direction) {
            if (direction === 'left') {
                this.$nextTick(() => {
                    const leftList = this.$refs.leftList;
                    if (leftList) {
                        const listBody = leftList.$el.querySelector('.ant-transfer-list-content');
                        if (listBody) {
                            const { scrollTop, clientHeight, scrollHeight } = listBody;
                            if (scrollTop + clientHeight >= scrollHeight - 50) {
                                if (!this.loading && this.hasMore) {
                                    this.getUsers(true);
                                }
                            }
                        }
                    }
                });
            }
        },
        handleFilter(inputValue, item) {
            return item.title.toLowerCase().indexOf(inputValue.toLowerCase()) !== -1;
        },
        handleLeftSearch(direction, value) {
            if(direction === 'left') {
                this.leftSearchValue = value;
                this.getUsers();
            }
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
        handleConfirm()
        {
            this.$refs.ruleForm.validate(validate => {
                if (validate) {
                    this.confirmLoading = true
                    const params = {
                        id: this.form.id,
                        groupName: this.form.name,
                        description: this.form.description,
                        userIds: this.selectedUserKeys,
                        joinGroup: this.auto ? '1' : '0'
                    }
                    const method = this.isEdit ? updateGroup : createGroup;
                    method(params).then(res => {
                        this.$emit('reset')
                        this.closeModal()
                    }).catch((err) => {
                        this.$notification["error"]({
                            message: err.response.data.error,
                        })
                    }).finally(() => {
                        this.confirmLoading = false
                    })
                }
            })
        },
        resetData() {
            this.page = 1;
            this.allUsers = [];
            this.hasMore = true;
            this.selectedUserKeys = [];
            this.leftSearchValue = '';
        },
    },
    created() {
        this.getUsers();
        // 如果初始数据不足，再次加载更多
        this.$nextTick(() => {
            if (this.allUsers.length < this.limit * 2 && this.hasMore) {
                this.getUsers(true);
            }
        });
    },
}
</script>

<style scoped lang="scss">
.transfer-container {
  margin: 20px 0;
  min-height: 300px;

  /deep/ .ant-transfer-list {
    width: 300px !important;
  }

  /deep/ .ant-transfer-list-header {
    padding-top: 8px;
    padding-bottom: 12px;
  }

  /deep/ .ant-transfer-list-body-search-wrapper {
    padding: 8px;
  }

  /deep/ .custom-transfer-search {
    width: 100%;

    .ant-input-prefix {
      color: rgba(0, 0, 0, 0.25);
    }

    .ant-input {
      border-radius: 4px;
    }
  }

  // 修改右侧列表样式
  /deep/ .ant-transfer-list:last-child {
    .ant-transfer-list-body-search-wrapper {
      display: none;
    }

    .ant-transfer-list-body {
      padding-top: 0;
    }

    .ant-transfer-list-content {
      height: calc(100% - 41px) !important; // 41px 是头部的高度
    }
  }
}

.description-input {
  width: 620px;
}

// 添加以下样式来移除 a-steps 的序号
/deep/ .ant-steps-item-icon {
  display: none !important;
}

/deep/ .ant-steps-item-content {
  margin-left: 0 !important;
}

/deep/ .ant-steps-item-tail {
  margin-left: 0 !important;
  padding-left: 0 !important;
}

/deep/ .ant-steps-item-title {
  padding-left: 0 !important;
}

// 如果您想保持步骤之间的连接线，可以添加以下样式
/deep/ .ant-steps-item::before {
  content: '';
  position: absolute;
  top: 16px;
  left: 0;
  width: 100%;
  height: 1px;
  background-color: #e8e8e8;
}
</style>