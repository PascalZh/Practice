#ifndef AGO_INL_
#define AGO_INL_
namespace ago {

  namespace utils {

    inline tuple<col_t, row_t> str2coord(const string &ind)
    {
      col_t col_ind = Action::column_indices.find( ind[0] );
      row_t row_ind = lexical_cast<row_t>(ind.substr(1));
      // eg. turn "19"(string) to 19(int)

      return std::make_tuple(col_ind, row_ind - 1);
    }

    inline bf reverse_bf(const bf&c)
    {
      assert(c == bf::white || c == bf::black || c == bf::none);
      return c == bf::white || c == bf::none ? bf::black : bf::white;
    }

  }

  inline board_flag &operator+=(board_flag &lhs, const int &rhs)
  {
    lhs = bf(int(lhs) + rhs);
    return lhs;
  }
  inline board_flag &operator-=(board_flag &lhs, const int &rhs)
  {
    lhs = bf(int(lhs) - rhs);
    return lhs;
  }

  inline Action::operator string() const
  {
    col_t col; row_t row;
    std::tie(col, row) = coord();
    string ret = fmt::to_string(row + 1);
    ret = column_indices[col] + ret;
    ret = (this->code < MAX_STONE_NUM?'b':'w') + ret;
    return ret;
  }

  inline tuple<col_t,row_t> Action::coord() const
  {
    assert(code < 2 * MAX_STONE_NUM);
    const auto &col = code % MAX_STONE_NUM / 19;
    const auto &row = code % MAX_STONE_NUM % 19;
    return std::make_tuple(col, row);
  }

  inline bf Action::color() const
  {
    assert(code <= 2 * MAX_STONE_NUM);
    if (code < MAX_STONE_NUM) return bf::black;
    else if (code < 2 * MAX_STONE_NUM) return bf::white;
    else if (code == 2 * MAX_STONE_NUM) return bf::none;
  }

  inline void Tree::erase_children(vector<Tree *>::iterator itr)
  {
    delete *itr;
    children.erase(itr);
  }

  inline void Tree::clear_children()
  {
    for (auto &m : children) {
      delete m;
    }
    children.clear();
  }

  inline bf &Board::operator [](const tuple<col_t,row_t> &coord)
  {
    const col_t &col_ind = std::get<0>(coord);
    const row_t &row_ind = std::get<1>(coord);
    assert(col_ind < 19 && row_ind < 19);
    return _board[col_ind][row_ind];
  }

  inline void AGoTree::init_nn()
  {
    is_nn_ready = true;
    p_c = new bp::child("./ago_nn.py", bp::std_in < task_out,
        bp::std_out > task_in);
  }

  inline void AGoTree::stop_nn()
  {
    p_c->wait();
    auto exit_code = p_c->exit_code();
    delete p_c;
  }

}

#endif /* #ifndef AGO_INL_ */