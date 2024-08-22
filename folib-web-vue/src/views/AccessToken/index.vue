<template>
  <div class="accessToken">
    <a-card :bordered="false" class="header-solid h-full">
      <a-row type="flex" justify="end">
        <a-col :span="2">
          <a-button type="primary" @click="createToken()" style="float: right">
            {{ $t('AccessToken.AddToken') }}
          </a-button>
        </a-col>
        <a-col :span="5" class="ml-10">
          <a-input-search v-model="query.tokenId" :placeholder="$t('AccessToken.QueryTokenId')" :allowClear=true
                          @search="searchToken()"/>
        </a-col>
      </a-row>
      <a-table
          :columns="i18nAccessTokenColumns"
          :data-source="tokenList"
          row-key="id"
          :loading="loading"
          :pagination="{ pageSize: query.pageSize, current: query.pageNum, total: total, showLessItems: true, showTotal: total => `共 ${total} 条` }"
          :scroll="{ x: true }"
          @change="handleChangeTable"
      >
        <template slot="operation" slot-scope="record">
          <div class="col-action by-flex">
            <a-popconfirm :title="$t('Setting.SureDelete')" okType="danger" :ok-text="$t('Setting.BeSure')"
                          :cancel-text="$t('Setting.Cancel')"
                          @confirm="handleDelete(record)">
              <a-button  type="link" size="small">
                <svg width="16" height="16" viewBox="0 0 20 20" fill="none" xmlns="http://www.w3.org/2000/svg">
                  <path class="fill-danger" fill-rule="evenodd" clip-rule="evenodd"
                        d="M9 2C8.62123 2 8.27497 2.214 8.10557 2.55279L7.38197 4H4C3.44772 4 3 4.44772 3 5C3 5.55228 3.44772 6 4 6L4 16C4 17.1046 4.89543 18 6 18H14C15.1046 18 16 17.1046 16 16V6C16.5523 6 17 5.55228 17 5C17 4.44772 16.5523 4 16 4H12.618L11.8944 2.55279C11.725 2.214 11.3788 2 11 2H9ZM7 8C7 7.44772 7.44772 7 8 7C8.55228 7 9 7.44772 9 8V14C9 14.5523 8.55228 15 8 15C7.44772 15 7 14.5523 7 14V8ZM12 7C11.4477 7 11 7.44772 11 8V14C11 14.5523 11.4477 15 12 15C12.5523 15 13 14.5523 13 14V8C13 7.44772 12.5523 7 12 7Z"
                        fill="#111827" />
                </svg>
              </a-button>
            </a-popconfirm>
          </div>
        </template>
      </a-table>
    </a-card>
    <CreateModal ref="modal"  @refreshData="queryList"></CreateModal>
  </div>
</template>
<script>
import CreateModal from './modal.vue'
import {deleteToken, tokenList} from '@/api/accessToken';

export default {
  name: "AccessToken",
  components: {
    CreateModal
  },
  data() {
    return {
      name: '',
      query: {
        pageSize: 10,
        pageNum: 1,
        tokenId: undefined,
      },
      total: 0,
      loading: false,
      tokenList: [],
      tokensListColumns: [
        {
          title: '  令牌标识',
          i18nKey: 'AccessToken.TokenId',
          dataIndex: 'tokenId',
          key: 'tokenId',
          align: 'center',
        },
        {
          title: '描述',
          i18nKey: 'AccessToken.Description',
          dataIndex: 'description',
          key: 'description',
          align: 'center',
          width: 200,
          customRender: (text) => {
            return text !== undefined && text !== null ? text : '--';
          }
        },
        {
          title: '所属用户',
          i18nKey: 'AccessToken.Username',
          dataIndex: 'username',
          key: 'username',
          align: 'center',
        },
        {
          title: '创建时间',
          i18nKey: 'AccessToken.CreateTime',
          dataIndex: 'createTime',
          key: 'createTime',
          align: 'center',
        },
        {
          title: '创建人',
          i18nKey: 'AccessToken.Operator',
          dataIndex: 'operator',
          key: 'operator',
          align: 'center',
        },
        {
          title: '失效时间',
          i18nKey: 'AccessToken.ExpireTime',
          dataIndex: 'expireTime',
          key: 'expireTime',
          align: 'center',
          customRender: (text) => {
            return text !== undefined && text !== null ? text : '永不过期';
          }
        },
        {
          title: '操作',
          i18nKey: 'AccessToken.Operation',
          align: 'center',
          scopedSlots: {customRender: 'operation'},
        },

      ]
    }
  },
  computed: {
    i18nAccessTokenColumns() {
      return this.tokensListColumns.map(column => {
        if (column.i18nKey) {
          column.title = this.$t(column.i18nKey);
        }
        return column;
      })
    }
  },
  mounted() {
    this.queryList()
  },
  methods: {

    createToken() {
      this.$refs.modal.openModal()
    },
    searchToken() {
      this.page = 1
      this.queryList()
    },
    handleDelete(record) {
      deleteToken({id:record.id,tokenId:record.tokenId}).then(res => {
        this.queryList()
      })
    },
    queryList() {
      this.loading = true
      tokenList(this.query).then(res => {
        if (res) {
          this.tokenList = res.list;
          this.total = res.total;
        }
      }).finally(() => {
        this.loading = false
      })
    },
    handleChangeTable(pagination) {
      this.query.pageNum = pagination.current
      this.queryList()
    },
  }
}
</script>

<style lang="scoped">

</style>