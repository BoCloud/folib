import AlertMixin from './alertMixin'
import ElementUI from 'element-ui';
import 'element-ui/lib/theme-chalk/index.css';

export default ({Vue}) => {
  Vue.use(AlertMixin)
  Vue.use(ElementUI)
}